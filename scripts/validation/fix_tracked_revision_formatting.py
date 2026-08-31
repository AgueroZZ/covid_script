#!/usr/bin/env python3
"""Restore original run formatting on tracked replacements in a DOCX file.

The tracked-replacement helper records insertions and deletions correctly, but
its generated runs may omit ``w:rPr``. Word then falls back to paragraph or
theme defaults, which can make revised text look different from the source
manuscript. This script copies the exact run properties from the original run
containing each deleted phrase to both sides of the tracked replacement.
"""

from __future__ import annotations

import argparse
import copy
import tempfile
import zipfile
from pathlib import Path

from lxml import etree


W_NS = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
XML_NS = "http://www.w3.org/XML/1998/namespace"
NS = {"w": W_NS}
W = f"{{{W_NS}}}"
XML_SPACE = f"{{{XML_NS}}}space"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("tracked_docx", type=Path)
    parser.add_argument("--reference", required=True, type=Path)
    parser.add_argument("--out", required=True, type=Path)
    return parser.parse_args()


def read_document_xml(docx_path: Path) -> etree._Element:
    with zipfile.ZipFile(docx_path) as archive:
        return etree.fromstring(archive.read("word/document.xml"))


def element_text(element: etree._Element, text_tag: str) -> str:
    return "".join(element.xpath(f".//w:{text_tag}/text()", namespaces=NS))


def matching_reference_run(
    reference_root: etree._Element, deleted_text: str
) -> tuple[etree._Element, str]:
    matches = []
    for text_node in reference_root.xpath("//w:t", namespaces=NS):
        source_text = text_node.text or ""
        if deleted_text in source_text:
            run = text_node.getparent()
            if run.tag != W + "r":
                continue
            properties = run.find(W + "rPr")
            if properties is not None:
                matches.append((properties, source_text))

    if len(matches) != 1:
        raise RuntimeError(
            f"Expected one formatted source run for {deleted_text!r}; "
            f"found {len(matches)}"
        )
    return matches[0]


def set_run_properties(change: etree._Element, properties: etree._Element) -> int:
    updated = 0
    for run in change.xpath("./w:r", namespaces=NS):
        existing = run.find(W + "rPr")
        if existing is not None:
            run.remove(existing)
        run.insert(0, copy.deepcopy(properties))
        updated += 1
    return updated


def make_space_run(properties: etree._Element) -> etree._Element:
    run = etree.Element(W + "r")
    run.append(copy.deepcopy(properties))
    text_node = etree.SubElement(run, W + "t")
    text_node.set(XML_SPACE, "preserve")
    text_node.text = " "
    return run


def sibling_text(element: etree._Element) -> str:
    return "".join(
        element.xpath(".//w:t/text() | .//w:delText/text()", namespaces=NS)
    )


def restore_boundary_spaces(
    deletion: etree._Element,
    insertion: etree._Element,
    properties: etree._Element,
    source_text: str,
    deleted_text: str,
) -> tuple[int, int]:
    """Restore source spaces outside the tracked replacement pair."""
    start = source_text.index(deleted_text)
    end = start + len(deleted_text)
    needs_leading = start > 0 and source_text[start - 1].isspace()
    needs_trailing = end < len(source_text) and source_text[end].isspace()
    parent = deletion.getparent()
    if parent is None or insertion.getparent() is not parent:
        raise RuntimeError("Tracked replacement pair has inconsistent parents")

    restored = 0
    normalized = 0

    if needs_leading:
        previous = deletion.getprevious()
        previous_text = sibling_text(previous) if previous is not None else ""
        if previous is not None and previous.tag == W + "ins" and previous_text.isspace():
            parent.replace(previous, make_space_run(properties))
            normalized += 1
        elif not previous_text or not previous_text[-1].isspace():
            parent.insert(parent.index(deletion), make_space_run(properties))
            restored += 1

    if needs_trailing:
        following = insertion.getnext()
        following_text = sibling_text(following) if following is not None else ""
        if not following_text or not following_text[0].isspace():
            parent.insert(parent.index(insertion) + 1, make_space_run(properties))
            restored += 1

    return restored, normalized


def apply_formatting(
    tracked_root: etree._Element, reference_root: etree._Element
) -> tuple[int, int, int, int]:
    replacements = 0
    formatted_runs = 0
    restored_spaces = 0
    normalized_space_revisions = 0

    for deletion in tracked_root.xpath("//w:del", namespaces=NS):
        deleted_text = element_text(deletion, "delText")
        if not deleted_text:
            continue

        sibling = deletion.getnext()
        if sibling is None or sibling.tag != W + "ins":
            raise RuntimeError(f"Deletion is not followed by an insertion: {deleted_text!r}")

        properties, source_text = matching_reference_run(reference_root, deleted_text)
        formatted_runs += set_run_properties(deletion, properties)
        formatted_runs += set_run_properties(sibling, properties)
        restored, normalized = restore_boundary_spaces(
            deletion, sibling, properties, source_text, deleted_text
        )
        restored_spaces += restored
        normalized_space_revisions += normalized
        replacements += 1

    return (
        replacements,
        formatted_runs,
        restored_spaces,
        normalized_space_revisions,
    )


def preserve_revision_boundary_whitespace(tracked_root: etree._Element) -> int:
    """Preserve leading and trailing spaces in paragraphs with revisions."""
    updated = 0
    for paragraph in tracked_root.xpath(
        "//w:p[.//w:ins or .//w:del]", namespaces=NS
    ):
        for text_node in paragraph.xpath(".//w:t | .//w:delText", namespaces=NS):
            value = text_node.text or ""
            if value and (value[0].isspace() or value[-1].isspace()):
                if text_node.get(XML_SPACE) != "preserve":
                    text_node.set(XML_SPACE, "preserve")
                    updated += 1
    return updated


def write_docx(source: Path, output: Path, document_xml: bytes) -> None:
    output.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(
        dir=output.parent, suffix=".docx", delete=False
    ) as handle:
        temporary = Path(handle.name)

    try:
        with zipfile.ZipFile(source) as src, zipfile.ZipFile(temporary, "w") as dst:
            for item in src.infolist():
                payload = (
                    document_xml
                    if item.filename == "word/document.xml"
                    else src.read(item.filename)
                )
                dst.writestr(item, payload)
        temporary.replace(output)
    finally:
        if temporary.exists():
            temporary.unlink()


def main() -> None:
    args = parse_args()
    tracked_root = read_document_xml(args.tracked_docx)
    reference_root = read_document_xml(args.reference)
    (
        replacements,
        formatted_runs,
        restored_spaces,
        normalized_space_revisions,
    ) = apply_formatting(tracked_root, reference_root)
    preserved_spaces = preserve_revision_boundary_whitespace(tracked_root)

    if replacements == 0:
        raise RuntimeError("No tracked replacements were found")

    payload = etree.tostring(
        tracked_root, xml_declaration=True, encoding="UTF-8", standalone=True
    )
    write_docx(args.tracked_docx, args.out, payload)
    print(
        f"[OK] wrote {args.out} "
        f"(replacements={replacements}, formatted_runs={formatted_runs}, "
        f"restored_spaces={restored_spaces}, "
        f"normalized_space_revisions={normalized_space_revisions}, "
        f"preserved_spaces={preserved_spaces})"
    )


if __name__ == "__main__":
    main()
