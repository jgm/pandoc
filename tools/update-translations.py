#!/usr/bin/env python3
# Update translations in data/translations
# based on data from Polyglossia and Babel.
#
# usage: python tools/update-translations.py

import json
import re
import subprocess
import sys
from configparser import ConfigParser
from importlib.util import module_from_spec, spec_from_file_location
from pathlib import Path
from shutil import rmtree

YAML = dict[str, str]
AST = dict[str, "AST"] | list["AST"] | str

# missing Listing
BABEL_KEYS = {
    "abstract": "Abstract",
    "also": "SeeAlso",
    "appendix": "Appendix",
    # "appendix.template",
    # [chapter].[ ][[appendix]], take appendix
    "bib": "Bibliography",
    "cc": "Cc",
    "chapter": "Chapter",
    # "chapter.template",
    # [chapter].[ ][[chapter]], take chapter
    # [[prechapter]] [chapter] [[postchapter]], take postchapter (cjk)
    "contents": "Contents",
    "encl": "Encl",
    "figure": "Figure",
    # "figure.template",
    # [figure].[ ][[figure]], take figure
    "glossary": "Glossary",
    "headto": "To",
    "index": "Index",
    "listfigure": "ListOfFigures",
    "listtable": "ListOfTables",
    "page": "Page",
    "part": "Part",
    # "part.template",
    # [part][ ][[part]], take part
    # [[prepart]] [part] [[postpart]], take postpart (cjk)
    # "postchapter",  # see chapter.template
    # "postpart",  # see part.template
    # "prechapter",  # see chapter.template
    "preface": "Preface",
    # "prepart",  # see part.template
    "proof": "Proof",
    "ref": "References",
    "see": "See",
    "table": "Table",
    # "table.template",
    # [table].[ ][[table]], take table
}

POLYGLOSSIA_KEYS = {
    "abstract": "Abstract",
    "also": "SeeAlso",
    "appendix": "Appendix",
    "bib": "Bibliography",
    "cc": "Cc",
    "chapter": "Chapter",
    "contents": "Contents",
    "encl": "Encl",
    "figure": "Figure",
    "glossary": "Glossary",
    "headto": "To",
    "index": "Index",
    "listfigure": "ListOfFigures",
    "listtable": "ListOfTables",
    "page": "Page",
    "part": "Part",
    "preface": "Preface",
    "proof": "Proof",
    "ref": "References",
    "see": "See",
    "table": "Table",
}


def git_clone(url: str, branch: str | None = None) -> Path:
    """Download the Git repository at the provided url."""
    return Path(
        subprocess.run(
            ["git", "clone", "--depth", "1", url]
            + ([] if branch is None else ["--branch", branch]),
            capture_output=True,
            text=True,
        ).stderr.split("'")[1]
    )


def parse_ast(tree: AST, is_map: bool = False):
    """Parse the pandoc value into a string."""
    if isinstance(tree, dict) and is_map:
        return {key: parse_ast(value) for key, value in tree.items()}
    elif isinstance(tree, list):
        return [parse_ast(value) for value in tree]
    elif isinstance(tree, str):
        return tree
    ast_type = tree["t"]
    assert ast_type in [
        "MetaInlines",
        "MetaString",
        "Str",
        "Space",
    ], f"Type {ast_type} is unsupported."
    if ast_type == "MetaInlines":
        return "".join(map(parse_ast, tree["c"]))
    elif ast_type == "Space":
        return " "
    else:
        return parse_ast(tree["c"])


def pandoc_parse(src: Path) -> YAML:
    """Parse YAML with pandoc's metadata parser."""
    with src.open() as f:
        data = f.read()
    try:
        # HACK: disable (most) markdown parsing of strings
        # TODO: commonmark, with unicode normalization disabled
        # https://github.com/jgm/pandoc/issues/8341
        format = "markdown-smart-subscript"
        pandoc_ast = json.loads(
            subprocess.run(
                ["pandoc", f"--from={format}", "--to=json"],
                input=f"---\n{data}\n---",
                capture_output=True,
                text=True,
                check=True,
            ).stdout
        )
    except subprocess.CalledProcessError as error:
        raise ValueError(error.stderr.strip())
    return parse_ast(pandoc_ast["meta"], is_map=True)  # type: ignore


def read_yaml(src: Path) -> YAML:
    """Read the YAML data with an ad hoc reader."""
    with src.open() as f:
        return {
            (tokens := line.split(": "))[0]: (
                value
                if not (value := ": ".join(tokens[1:]).strip()).startswith("'")
                or not value.endswith("'")
                else value[1:-1]
            )
            for line in f.readlines()
        }


def save_yaml(data: YAML, dst: Path) -> None:
    """Save the YAML data with an ad hoc writer."""
    with dst.open("w") as f:
        f.write(
            "\n".join(
                f"{key}: {value}"
                for key, value in sorted(data.items(), key=lambda x: x[0])
            ) + "\n"
        )
    assert pandoc_parse(dst) == data, "Serialized different from expected."


def parse_babel(src: Path) -> tuple[str, set[str], YAML]:
    """Parse Babel's language files."""
    # TODO: strict=True once https://github.com/latex3/babel/pull/303 lands
    config = ConfigParser(strict=False)
    config.read(src)
    bcp47tag = config["identification"]["tag.bcp47"]
    captions = config["captions"]
    # HACK: manual modifications (see BABEL_KEYS)
    if "postchapter" in captions:
        captions["chapter"] = captions["postchapter"]
    if "postpart" in captions:
        captions["part"] = captions["postpart"]
    data = {
        BABEL_KEYS[key]: value if not value.endswith(":") else value[:-1]
        for key, value in captions.items()
        if key in BABEL_KEYS and len(value) > 0 and value != "<++>"
    }
    return bcp47tag, set(captions.keys()), data


def parse_braces(s: str, i: int) -> str:
    """Return the contiguous block starting at index i defined by braces."""
    assert s[i] == "{", f"{s} at character {i} does not start with a brace."
    left = 0
    for j in range(i, len(s)):
        ch = s[j]
        if ch == "{":
            left += 1
        elif ch == "}":
            left -= 1
            if left == 0:
                return s[i : j + 1]
    raise ValueError(f"{s} is mismatched.")


def parse_polyglossia_value(value: str) -> str:
    """Parse the Polyglossia value."""
    patterns = [
        re.compile(r"\\@ensure@RTL\{(.*)\}"),
        re.compile(r".*##1##2\s*(.*)"),
        re.compile(r"\\textsc\{(.*)\}"),
    ]
    for pattern in patterns:
        match = pattern.match(value)
        if match is not None:
            return match.group(1)
    if "xpg@hr@digraph" in value:
        value = re.sub(
            r"\\xpg@hr@digraph\{(.)\}\{(.)\}",
            lambda match: match.group(1) + match.group(2),
            value,
        )
    return value.replace("\\", "").replace(":", "").strip()


def parse_polyglossia(data: list[tuple[str, str]]) -> YAML:
    """Process the Polyglossia data."""
    return {
        POLYGLOSSIA_KEYS[key]: parse_polyglossia_value(value)
        for key, value in data
        if key in POLYGLOSSIA_KEYS
        and value not in ["", r"$\rightarrow$", r"$\Rightarrow$"]
    }


def parse_ldf(src: Path) -> list[tuple[str, dict[str, str], YAML]]:
    """Parse Polyglossia's language definition files."""
    with src.open() as f:
        data = f.read()
    # HACK: regex parsing of latex
    pattern = re.compile(r"(?m:^)[^%]*\\def\\([^\{]+)name[^\{]*\{(.*)\}")
    languages = []
    extra = []
    for match in re.finditer(
        r"\\def\\captions@?([^@\{]*)@?([^@\{]*)@?([^@\{]*)\{", data
    ):
        language, variant, script = match.groups()
        options = {}
        if len(variant) > 0:
            options["variant"] = variant
        if len(script) > 0:
            options["script"] = script
        body = parse_braces(data, match.end() - 1)
        matches = pattern.findall(body)
        lines = [
            line
            for line in body.strip().splitlines()
            if "def" in line
            and "name" in line
            and not line.strip().startswith("%")
        ]
        assert len(lines) == len(matches), "Missing matches."
        captions = parse_polyglossia(matches)
        languages.append((language, options, captions))
        if len(options) == 1 and "variant" in options:
            extra.append((language, {"script": options["variant"]}, captions))
    assert len(languages) == data.count(r"\def\captions"), "Missing captions."
    return languages + extra


def get_tags(
    bcp472lang: dict[str, str], bcp472opts: dict[str, str], language: str
) -> dict[str, dict[str, str]]:
    """Get the bcp47 tags and options matching the language."""
    bcp47tags = {}
    for bcp47tag, name in bcp472lang.items():
        if name == language:
            options = (
                bcp472opts[bcp47tag].lower().split(",")
                if bcp47tag in bcp472opts
                else ()
            )
            bcp47tags[bcp47tag] = {
                s[0]: s[1] for s in map(lambda s: s.split("="), options)
            }
    return bcp47tags


if __name__ == "__main__":
    translations = Path("data/translations")
    translations.mkdir(parents=True, exist_ok=True)
    for translation in translations.rglob("*.yaml"):
        assert read_yaml(translation) == pandoc_parse(
            translation
        ), f"Pandoc parsing doesn't match on {translation}."

    # original repository: https://github.com/latex3/babel/
    babel = git_clone(
        "https://github.com/stephen-huan/babel/", branch="pandoc"
    )
    # original repository: https://github.com/reutenauer/polyglossia/
    polyglossia = git_clone(
        "https://github.com/stephen-huan/polyglossia/", branch="pandoc"
    )

    babel_data = {}
    babel_keys = set()
    for path in (babel / "locale").glob("*/*.ini"):
        bcp47tag, keys, data = parse_babel(path)
        babel_keys |= keys
        babel_data[bcp47tag] = data
    # print(f"Babel keys: {babel_keys}")

    # https://docs.python.org/3/library/importlib.html#importing-a-source-file-directly
    spec = spec_from_file_location("bcp47", polyglossia / "tools" / "bcp47.py")
    assert spec is not None and spec.loader is not None, "Can't find bcp47.py."
    bcp47 = module_from_spec(spec)
    sys.modules["bcp47"] = bcp47
    spec.loader.exec_module(bcp47)

    # check coverage of bcp47.bcp472lang against actual polyglossia/tex/*.ldf
    known = bcp47.bcp472lang.keys()
    assert set(bcp47.babelname2bcp47.values()) <= known, "Some values missing."
    known |= bcp47.babelname2bcp47.keys()
    known |= set(bcp47.bcp472lang.values())
    for path in (polyglossia / "tex").glob("*.ldf"):
        name = path.stem.split("-")[1]
        assert name in known or name == "latex", f"{name} not found."

    polyglossia_data = {}
    polyglossia_keys = set()
    for name in sorted(set(bcp47.bcp472lang.values())):
        bcp47tags = get_tags(bcp47.bcp472lang, bcp47.bcp472opts, name)
        ldf = parse_ldf(polyglossia / "tex" / f"gloss-{name}.ldf")
        if len(ldf) > 0:
            languages, _, entries = zip(*ldf)
            for data in entries:
                polyglossia_keys |= data.keys()
            assert {name} == set(languages), "Languages don't match."
        for bcp47tag, options in bcp47tags.items():
            polyglossia_data[bcp47tag] = {}
            # go in order of specificity and only apply if more general
            for language, opt, data in sorted(ldf, key=lambda x: len(x[1])):
                if opt.keys() <= options.keys() and all(
                    value == options[key] for key, value in opt.items()
                ):
                    polyglossia_data[bcp47tag] |= data
    # print(f"Polyglossia keys: {polyglossia_keys}")

    # bcp47tag unique to either Babel or Polyglossia
    unique = babel_data.keys() ^ polyglossia_data.keys()
    shared = babel_data.keys() & polyglossia_data.keys()
    assert (
        unique | shared == babel_data.keys() | polyglossia_data.keys()
    ), "Missing keys."
    for bcp47tag in unique:
        data, other_data = (
            (babel_data, polyglossia_data)
            if bcp47tag in babel_data
            else (polyglossia_data, babel_data)
        )
        assert bcp47tag not in other_data, "Shared key."
        captions = data[bcp47tag]
        if len(captions) > 0:
            save_yaml(captions, translations / f"{bcp47tag}.yaml")
    # merge Babel and Polyglossia data
    for bcp47tag in shared:
        data, other_data = (
            (babel_data, polyglossia_data)
            if len(babel_data[bcp47tag]) >= len(polyglossia_data[bcp47tag])
            else (polyglossia_data, babel_data)
        )
        # prefer values from data over other_data
        captions = other_data[bcp47tag] | data[bcp47tag]
        if len(captions) > 0:
            save_yaml(captions, translations / f"{bcp47tag}.yaml")

    # clean up after ourselves
    rmtree(babel)
    rmtree(polyglossia)
