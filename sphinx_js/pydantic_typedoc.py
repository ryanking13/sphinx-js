import re
from collections.abc import Iterator
from inspect import isclass
from os.path import basename, relpath, sep, splitext
from typing import Annotated, Any, Literal, Optional, Protocol, TypedDict, cast

from pydantic import BaseConfig, BaseModel, Field, ValidationError

from . import ir
from .analyzer_utils import is_explicitly_rooted


class Analyzer(Protocol):
    _index: dict[int, "IndexType"]
    _base_dir: str


class Source(BaseModel):
    fileName: str
    line: int


class Comment(BaseModel):
    returns: str = ""
    shortText: str | None
    text: str | None


class Flags(BaseModel):
    isAbstract: bool = False
    isExported: bool = False
    isOptional: bool = False
    isPrivate: bool = False
    isRest: bool = False
    isStatic: bool = False


class Base(BaseModel):
    children: list["Node"] = []
    id: int | None
    kindString: str = ""
    originalName: str | None
    sources: list[Source] = []
    parent: Optional["IndexType"]
    flags: Flags = Field(default_factory=Flags)

    class Config(BaseConfig):
        fields = {"parent": {"exclude": True}}  # type:ignore[dict-item]

    def member_properties(self) -> dict[str, bool]:
        return dict(
            is_abstract=self.flags.isAbstract,
            is_optional=self.flags.isOptional,
            is_static=self.flags.isStatic,
            is_private=self.flags.isPrivate,
        )

    def _parent_nodes(self) -> Iterator["ExternalModule | OtherNode"]:
        """Return an iterator of parent nodes"""
        n: IndexType | None = cast(IndexType, self)
        while n and n.id != 0:
            if n.kindString == "External module" or n.kindString == "Module":
                # Found one!
                yield n
            n = n.parent

    def _containing_module(self, base_dir: str) -> ir.Pathname | None:
        """Return the Pathname pointing to the module containing the given
        node, None if one isn't found."""
        for n in self._parent_nodes():
            return ir.Pathname(n.make_path_segments(base_dir))
        return None

    def _containing_deppath(self) -> str | None:
        """Return the path pointing to the module containing the given node.
        The path is absolute or relative to `root_for_relative_js_paths`.
        Raises ValueError if one isn't found.

        """
        return self.sources[0].fileName

    def _path_segments(self, base_dir: str) -> list[str]:
        raise NotImplementedError

    # Optimization: Could memoize this for probably a decent perf gain: every child
    # of an object redoes the work for all its parents.
    def make_path_segments(
        self, base_dir: str, child_was_static: bool | None = None
    ) -> list[str]:
        """Return the full, unambiguous list of path segments that points to an
        entity described by a TypeDoc JSON node.

        Example: ``['./', 'dir/', 'dir/', 'file.', 'object.', 'object#', 'object']``

        :arg base_dir: Absolute path of the dir relative to which file-path
            segments are constructed
        :arg child_was_static: True if the child node we're computing the path of
            is a static member of the node under consideration. False if it is.
            None if the current node is the one we're ultimately computing the path
            of.

        TypeDoc uses a totally different, locality-sensitive resolution mechanism
        for links: https://typedoc.org/guides/link-resolution/. It seems like a
        less well thought-out system than JSDoc's namepaths, as it doesn't
        distinguish between, say, static and instance properties of the same name.
        (AFAICT, TypeDoc does not emit documentation for inner properties, as for a
        function nested within another function.) We're sticking with our own
        namepath-like paths, even if we eventually support {@link} syntax.

        """
        node_is_static = self.flags.isStatic

        parent_segments = (
            self.parent.make_path_segments(base_dir, child_was_static=node_is_static)
            if self.parent
            else []
        )

        if child_was_static is None:
            delimiter = ""
        elif not child_was_static and self.kindString == "Class":
            delimiter = "#"
        else:
            delimiter = "."

        segments = self._path_segments(base_dir)

        if segments:
            # It's not some abstract thing the user doesn't think about and we skip
            # over.
            segments[-1] += delimiter
            result = parent_segments + segments
        else:
            # Allow some levels of the JSON to not have a corresponding path segment:
            result = parent_segments
        return result


class Root(Base):
    # These are probably never present except "name"
    kindString: Literal["root"] = "root"
    name: str | None

    def _path_segments(self, base_dir: str) -> list[str]:
        return []


class TopLevelPropertiesDict(TypedDict):
    name: str
    path: ir.Pathname
    filename: str
    deppath: str | None
    description: str
    line: int
    deprecated: bool
    examples: list[str]
    see_alsos: list[str]
    properties: list[ir.Attribute]
    exported_from: ir.Pathname | None


class TopLevelProperties(Base):
    name: str
    kindString: str
    comment: Comment = Field(default_factory=Comment)

    def short_name(self) -> str:
        """Overridden by Modules and Namespaces to strip quotes."""
        return self.name

    def _top_level_properties(self, base_dir: str) -> TopLevelPropertiesDict:
        source = self.sources[0]
        if self.flags.isExported:
            exported_from = self._containing_module(base_dir)
        else:
            exported_from = None
        return dict(
            name=self.short_name(),
            path=ir.Pathname(self.make_path_segments(base_dir)),
            filename=basename(source.fileName),
            deppath=self._containing_deppath(),
            description=make_description(self.comment),
            line=source.line,
            # These properties aren't supported by TypeDoc:
            deprecated=False,
            examples=[],
            see_alsos=[],
            properties=[],
            exported_from=exported_from,
        )

    def to_ir(self, analyzer: Analyzer) -> tuple[ir.TopLevel | None, list["Node"]]:
        return None, self.children


class NodeBase(TopLevelProperties):
    sources: list[Source]

    def _path_segments(self, base_dir: str) -> list[str]:
        return [self.name]


class Accessor(NodeBase):
    kindString: Literal["Accessor"]
    getSignature: list["Signature"] = []
    setSignature: list["Signature"] = []

    def to_ir(self, analyzer: Analyzer) -> tuple[ir.Attribute, list["Node"]]:
        if self.getSignature:
            # There's no signature to speak of for a getter: only a return type.
            type = self.getSignature[0].type
        else:
            # ES6 says setters have exactly 1 param. I'm not sure if they
            # can have multiple signatures, though.
            type = self.setSignature[0].parameters[0].type
        res = ir.Attribute(
            type=type.render_name(analyzer._index),
            **self.member_properties(),
            **self._top_level_properties(analyzer._base_dir),
        )
        return res, self.children


class Callable(NodeBase):
    kindString: Literal[
        "Constructor",
        "Method",
        "Function",
    ]
    signatures: list["Signature"] = []

    def _path_segments(self, base_dir: str) -> list[str]:
        return []

    def to_ir(self, analyzer: Analyzer) -> tuple[ir.TopLevel | None, list["Node"]]:
        # There's really nothing in these; all the interesting bits are in
        # the contained 'Call signature' keys. We support only the first
        # signature at the moment, because to do otherwise would create
        # multiple identical pathnames to the same function, which would
        # cause the suffix tree to raise an exception while being built. An
        # eventual solution might be to store the signatures in a one-to-
        # many attr of Functions.
        first_sig = self.signatures[0]  # Should always have at least one
        first_sig.sources = self.sources
        return first_sig.to_ir(analyzer)


class ClassOrInterface(NodeBase):
    kindString: Literal["Class", "Interface"]
    extendedTypes: list["TypeD"] = []
    implementedTypes: list["TypeD"] = []

    def _related_types(
        self,
        analyzer: Analyzer,
        kind: Literal["extendedTypes", "implementedTypes"],
    ) -> list[ir.Pathname]:
        """Return the unambiguous pathnames of implemented interfaces or
        extended classes.

        If we encounter a formulation of interface or class reference that we
        don't understand (which I expect to occur only if it turns out you can
        use a class or interface literal rather than referencing a declared
        one), return 'UNIMPLEMENTED' for that interface or class so somebody
        files a bug requesting we fix it. (It's not worth crashing for.)

        """
        types = []
        if kind == "extendedTypes":
            orig_types = self.extendedTypes
        elif kind == "implementedTypes":
            assert self.kindString == "Class"
            orig_types = self.implementedTypes
        else:
            raise ValueError(
                f"Expected kind to be 'extendedTypes' or 'implementedTypes' not {kind}"
            )

        for t in orig_types:
            if t.type == "reference" and t.id is not None:
                rtype = analyzer._index[t.id]
                pathname = ir.Pathname(rtype.make_path_segments(analyzer._base_dir))
                types.append(pathname)
            # else it's some other thing we should go implement
        return types

    def _constructor_and_members(
        self, analyzer: Analyzer
    ) -> tuple[ir.Function | None, list[ir.Function | ir.Attribute]]:
        """Return the constructor and other members of a class.

        In TS, a constructor may have multiple (overloaded) type signatures but
        only one implementation. (Same with functions.) So there's at most 1
        constructor to return. Return None for the constructor if it is
        inherited or implied rather than explicitly present in the class.

        :arg cls: A TypeDoc node of the class to take apart
        :return: A tuple of (constructor Function, list of other members)

        """
        constructor = None
        members = []
        for child in self.children:
            result, _ = child.to_ir(analyzer)
            if not result:
                continue
            if child.kindString == "Constructor":
                # This really, really should happen exactly once per class.
                assert isinstance(result, ir.Function)
                constructor = result
            else:
                assert isinstance(result, (ir.Function, ir.Attribute))
                members.append(result)
        return constructor, members


class Class(ClassOrInterface):
    kindString: Literal["Class"]

    def to_ir(self, analyzer: Analyzer) -> tuple[ir.Class | None, list["Node"]]:
        constructor, members = self._constructor_and_members(analyzer)
        result = ir.Class(
            constructor=constructor,
            members=members,
            supers=self._related_types(analyzer, kind="extendedTypes"),
            is_abstract=self.flags.isAbstract,
            interfaces=self._related_types(analyzer, kind="implementedTypes"),
            **self._top_level_properties(analyzer._base_dir),
        )
        return result, self.children


class Interface(ClassOrInterface):
    kindString: Literal["Interface"]

    def to_ir(self, analyzer: Analyzer) -> tuple[ir.Interface, list["Node"]]:
        _, members = self._constructor_and_members(analyzer)
        result = ir.Interface(
            members=members,
            supers=self._related_types(analyzer, kind="extendedTypes"),
            **self._top_level_properties(analyzer._base_dir),
        )
        return result, self.children


class Member(NodeBase):
    kindString: Literal[
        "Property",
        "Variable",
    ]
    type: "TypeD"

    def to_ir(self, analyzer: Analyzer) -> tuple[ir.TopLevel | None, list["Node"]]:
        result = ir.Attribute(
            type=self.type.render_name(analyzer._index),
            **self.member_properties(),
            **self._top_level_properties(analyzer._base_dir),
        )
        return result, self.children


class ExternalModule(NodeBase):
    kindString: Literal["External module", "Module"]
    originalName: str = ""

    def short_name(self) -> str:
        return self.name[1:-1]  # strip quotes

    def _path_segments(self, base_dir: str) -> list[str]:
        # 'name' contains folder names if multiple folders are passed into
        # TypeDoc. It's also got excess quotes. So we ignore it and take
        # 'originalName', which has a nice, absolute path.
        if not self.originalName:
            return []
        rel = relpath(self.originalName, base_dir)
        if not is_explicitly_rooted(rel):
            rel = f".{sep}{rel}"

        segments = rel.split(sep)
        filename = splitext(segments[-1])[0]
        return [s + "/" for s in segments[:-1]] + [filename]


class OtherNode(NodeBase):
    kindString: Literal["Enumeration", "Enumeration member", "Namespace", "Type alias"]


Node = Annotated[
    Accessor | Callable | Class | ExternalModule | Interface | Member | OtherNode,
    Field(discriminator="kindString"),
]


def make_description(comment: Comment) -> str:
    """Construct a single comment string from a fancy object."""
    ret = "\n\n".join(text for text in [comment.shortText, comment.text] if text)
    return ret.strip()


class Param(Base):
    kindString: Literal["Parameter"] = "Parameter"
    comment: Comment = Field(default_factory=Comment)
    defaultValue: str | None
    flags: Flags
    name: str
    type: "TypeD"

    def to_ir(self, index: dict[int, "IndexType"]) -> ir.Param:
        """Make a Param from a 'parameters' JSON item"""
        default = self.defaultValue or ir.NO_DEFAULT
        return ir.Param(
            name=self.name,
            description=make_description(self.comment),
            has_default=self.defaultValue is not None,
            is_variadic=self.flags.isRest,
            # For now, we just pass a single string in as the type rather than
            # a list of types to be unioned by the renderer. There's really no
            # disadvantage.
            type=self.type.render_name(index),
            default=default,
        )


class Signature(TopLevelProperties):
    kindString: Literal[
        "Constructor signature", "Call signature", "Get signature", "Set signature"
    ]
    parent: "Node" = None  # type:ignore[assignment]

    name: str
    parameters: list["Param"] = []
    sources: list[Source] = []
    type: "TypeD"
    inheritedFrom: Any = None

    def _path_segments(self, base_dir: str) -> list[str]:
        return [self.parent.name]

    def return_type(self, index: dict[int, "IndexType"]) -> list[ir.Return]:
        """Return the Returns a function signature can have.

        Because, in TypeDoc, each signature can have only 1 @return tag, we
        return a list of either 0 or 1 item.

        """
        type = self.type
        if type.type == "intrinsic" and type.name == "void":
            # Returns nothing
            return []
        return [
            ir.Return(
                type=type.render_name(index),
                description=self.comment.returns.strip(),
            )
        ]

    def to_ir(self, analyzer: Analyzer) -> tuple[ir.Function | None, list["Node"]]:
        if self.inheritedFrom is not None:
            return None, []
        # This is the real meat of a function, method, or constructor.
        #
        # Constructors' .name attrs end up being like 'new Foo'. They
        # should probably be called "constructor", but I'm not bothering
        # with that yet because nobody uses that attr on constructors atm.
        result = ir.Function(
            params=[p.to_ir(analyzer._index) for p in self.parameters],
            # Exceptions are discouraged in TS as being unrepresentable in its
            # type system. More importantly, TypeDoc does not support them.
            exceptions=[],
            # Though perhaps technically true, it looks weird to the user
            # (and in the template) if constructors have a return value:
            returns=self.return_type(analyzer._index)
            if self.kindString != "Constructor signature"
            else [],
            **self.parent.member_properties(),
            **self._top_level_properties(analyzer._base_dir),
        )
        return result, self.children


class TypeBase(Base):
    typeArguments: list["TypeD"] = []

    def render_name(self, index: dict[int, "IndexType"]) -> str:
        name = self._render_name_root(index)

        if self.typeArguments:
            arg_names = ", ".join(arg.render_name(index) for arg in self.typeArguments)
            name += f"<{arg_names}>"

        return name

    def _render_name_root(self, index: dict[int, "IndexType"]) -> str:
        raise NotImplementedError


class AndOrType(TypeBase):
    type: Literal["union", "intersection"]
    types: list["TypeD"]

    def _render_name_root(self, index: dict[int, "IndexType"]) -> str:
        if self.type == "union":
            return "|".join(t.render_name(index) for t in self.types)
        elif self.type == "intersection":
            return " & ".join(t.render_name(index) for t in self.types)


class ArrayType(TypeBase):
    type: Literal["array"]
    elementType: "TypeD"

    def _render_name_root(self, index: dict[int, "IndexType"]) -> str:
        return self.elementType.render_name(index) + "[]"


class OperatorType(TypeBase):
    type: Literal["typeOperator"]
    operator: str
    target: "TypeD"

    def _render_name_root(self, index: dict[int, "IndexType"]) -> str:
        return self.operator + " " + self.target.render_name(index)


class ParameterType(TypeBase):
    type: Literal["typeParameter"]
    name: str
    constraint: Optional["TypeD"]

    def _render_name_root(self, index: dict[int, "IndexType"]) -> str:
        name = self.name
        if self.constraint is not None:
            name += " extends " + self.constraint.render_name(index)
            # e.g. K += extends + keyof T
        return name


class ReferenceType(TypeBase):
    type: Literal["reference", "intrinsic"]
    name: str
    id: int | None

    def _render_name_root(self, index: dict[int, "IndexType"]) -> str:
        # test_generic_member() (currently skipped) tests this.
        if self.id:
            node = index[self.id]
            assert node.name
        return self.name


class ReflectionType(TypeBase):
    type: Literal["reflection"]

    def _render_name_root(self, index: dict[int, "IndexType"]) -> str:
        return "<TODO: reflection>"


class StringLiteralType(TypeBase):
    type: Literal["stringLiteral"]
    name: str
    value: str

    def _render_name_root(self, index: dict[int, "IndexType"]) -> str:
        return f'"{self.value}"'


class TupleType(TypeBase):
    type: Literal["tuple"]
    elements: list["TypeD"]

    def _render_name_root(self, index: dict[int, "IndexType"]) -> str:
        types = [t.render_name(index) for t in self.elements]
        return "[" + ", ".join(types) + "]"


class UnknownType(TypeBase):
    type: Literal["unknown"]
    name: str

    def _render_name_root(self, index: dict[int, "IndexType"]) -> str:
        if re.match(r"-?\d*(\.\d+)?", self.name):  # It's a number.
            # TypeDoc apparently sticks numeric constants' values into the
            # type name. String constants? Nope. Function ones? Nope.
            return "number"
        return self.name


AnyNode = Node | Root | Signature


Type = (
    AndOrType
    | ArrayType
    | OperatorType
    | ParameterType
    | ReferenceType
    | ReflectionType
    | StringLiteralType
    | TupleType
    | UnknownType
)

TypeD = Annotated[Type, Field(discriminator="TypeD")]

IndexType = Node | Root | Signature | Param


for cls in list(globals().values()):
    if isclass(cls) and issubclass(cls, BaseModel):
        cls.update_forward_refs()


def parse(json: dict[str, Any]) -> Root:
    try:
        return Root.parse_obj(json)
    except ValidationError as exc:
        fix_exc_errors(json, exc)
        raise


# Fix error messages
#
# The Pydantic error messages tend to contain TONS of irrelevant stuff. This
# deletes the useless stuff and adds important context.

discriminators = ["kindString", "type"]
classesByDiscriminator: dict[str, dict[str, type[BaseModel]]] = {
    disc: {} for disc in discriminators
}

for cls in list(globals().values()):
    if not isclass(cls) or not issubclass(cls, BaseModel):
        continue
    for disc in discriminators:
        if disc not in cls.__annotations__:
            continue
        ann = cls.__annotations__[disc]
        if getattr(ann, "__name__", None) != "Literal":
            continue
        for arg in ann.__args__:
            classesByDiscriminator[disc][arg] = cls


def fix_exc_errors(json: Any, exc: ValidationError) -> None:
    from pprint import pprint

    s = sorted(
        set(e["loc"][:-1] for e in exc.errors()), key=lambda loc: (-len(loc), loc)
    )
    handled = set()
    errors = []
    for loc in s:
        if loc in handled:
            continue
        # Add all prefixes of the current loc to handled
        for i in range(len(s)):
            handled.add(loc[:i])

        # follow path to get problematic subobject
        o = json
        for a in loc:
            o = o[a]

        # Work out the discriminator and use it to look up the appropriate class
        for disc in discriminators:
            if disc not in o:
                continue
            if o[disc] not in classesByDiscriminator[disc]:
                continue
            c = classesByDiscriminator[disc][o[disc]]
            break
        else:
            print("Unable to locate discriminator for object:")
            pprint(o, depth=1)
            continue

        try:
            c.parse_obj(o)
        except ValidationError as e:
            errs = e.errors()
        else:
            continue

        for err in errs:
            # Extend the loc so that it is relative to the top level object
            err["loc"] = loc + err["loc"]
            err["msg"] += "\n" + f"  Discriminator: {disc} = {o[disc]}\n"
            print("\n")
            print("loc:", err["loc"])
            print("msg:", err["msg"])
            print("obj:")
            pprint(o, depth=1)
            print("\n")
        errors.extend(errs)

    if errors:
        exc._error_cache = errors
