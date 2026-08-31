#!/usr/bin/env python3
"""Create a tracked-change Word copy with the corrected Figure 2 and caption."""

from __future__ import annotations

import argparse
import copy
import datetime as dt
import io
import os
import shutil
import tempfile
import zipfile
from pathlib import Path

from lxml import etree
from PIL import Image


W_NS = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
R_NS = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
A_NS = "http://schemas.openxmlformats.org/drawingml/2006/main"
WP_NS = "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
PIC_NS = "http://schemas.openxmlformats.org/drawingml/2006/picture"
PKG_REL_NS = "http://schemas.openxmlformats.org/package/2006/relationships"
XML_NS = "http://www.w3.org/XML/1998/namespace"

NS = {
    "w": W_NS,
    "r": R_NS,
    "a": A_NS,
    "wp": WP_NS,
    "pic": PIC_NS,
}


CAPTION = (
    "Figure 2. Posterior median P-scores for the initial and Delta waves across "
    "33 Eurostat geographies, England and Wales (labelled UK in the map), and "
    "Ireland (IE), stratified by displayed age group."
)

AGE_NOTE = (
    "England-and-Wales estimates use the Under 65 source band in the displayed "
    "Ages 40-59 panels and the Ages 65-84 source band in the displayed Ages "
    "60-79 panels. Ireland estimates use source bands Ages 45-64 and Ages 65-84 "
    "in the displayed Ages 40-59 and Ages 60-79 panels, respectively. These are "
    "closest-available approximations rather than harmonized age-specific estimates."
)


def qn(namespace: str, local_name: str) -> str:
    """Return a Clark-notation XML name."""
    return f"{{{namespace}}}{local_name}"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", required=True, type=Path)
    parser.add_argument("--figure", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--author", default="Codex")
    return parser.parse_args()


def next_revision_id(root: etree._Element) -> int:
    values = []
    for element in root.xpath("//*[@w:id]", namespaces=NS):
        value = element.get(qn(W_NS, "id"))
        if value is not None and value.lstrip("-").isdigit():
            values.append(int(value))
    return max(values, default=0) + 1


def revision_element(
    tag: str,
    revision_id: int,
    author: str,
    timestamp: str,
) -> etree._Element:
    element = etree.Element(qn(W_NS, tag))
    element.set(qn(W_NS, "id"), str(revision_id))
    element.set(qn(W_NS, "author"), author)
    element.set(qn(W_NS, "date"), timestamp)
    return element


def paragraph_visible_text(paragraph: etree._Element) -> str:
    return "".join(paragraph.xpath(".//w:t/text()", namespaces=NS))


def find_unique_paragraph(root: etree._Element, prefix: str) -> etree._Element:
    matches = [
        paragraph
        for paragraph in root.xpath("//w:body/w:p", namespaces=NS)
        if paragraph_visible_text(paragraph).startswith(prefix)
    ]
    if len(matches) != 1:
        raise RuntimeError(
            f"Expected one paragraph beginning with {prefix!r}; found {len(matches)}."
        )
    return matches[0]


def deletion_run(run: etree._Element) -> etree._Element:
    deleted = copy.deepcopy(run)
    for text_node in deleted.xpath(".//w:t", namespaces=NS):
        text_node.tag = qn(W_NS, "delText")
    return deleted


def tracked_text_replacement(
    paragraph: etree._Element,
    new_text: str,
    revision_id: int,
    author: str,
    timestamp: str,
) -> int:
    runs = paragraph.xpath("./w:r", namespaces=NS)
    if not runs:
        raise RuntimeError("Tracked text replacement requires at least one source run.")

    insertion_index = 1 if paragraph.find(qn(W_NS, "pPr")) is not None else 0
    for run in runs:
        paragraph.remove(run)

    deletion = revision_element("del", revision_id, author, timestamp)
    for run in runs:
        deletion.append(deletion_run(run))
    paragraph.insert(insertion_index, deletion)

    insertion = revision_element("ins", revision_id + 1, author, timestamp)
    new_run = etree.Element(qn(W_NS, "r"))
    source_properties = runs[0].find(qn(W_NS, "rPr"))
    if source_properties is not None:
        new_run.append(copy.deepcopy(source_properties))
    new_text_node = etree.SubElement(new_run, qn(W_NS, "t"))
    new_text_node.set(qn(XML_NS, "space"), "preserve")
    new_text_node.text = new_text
    insertion.append(new_run)
    paragraph.insert(insertion_index + 1, insertion)
    return revision_id + 2


def next_relationship_id(relationships: etree._Element) -> str:
    existing = []
    for relationship in relationships:
        value = relationship.get("Id", "")
        if value.startswith("rId") and value[3:].isdigit():
            existing.append(int(value[3:]))
    return f"rId{max(existing, default=0) + 1}"


def next_doc_property_id(root: etree._Element) -> int:
    values = []
    for element in root.xpath("//wp:docPr", namespaces=NS):
        value = element.get("id")
        if value is not None and value.isdigit():
            values.append(int(value))
    return max(values, default=0) + 1


def tracked_image_replacement(
    image_paragraph: etree._Element,
    new_relationship_id: str,
    figure_path: Path,
    revision_id: int,
    author: str,
    timestamp: str,
    new_doc_property_id: int,
) -> int:
    runs = image_paragraph.xpath("./w:r[.//a:blip]", namespaces=NS)
    if len(runs) != 1:
        raise RuntimeError(f"Expected one image run in Figure 2 paragraph; found {len(runs)}.")
    old_run = runs[0]
    new_run = copy.deepcopy(old_run)

    blip = new_run.xpath(".//a:blip", namespaces=NS)[0]
    blip.set(qn(R_NS, "embed"), new_relationship_id)

    inline = new_run.xpath(".//wp:inline", namespaces=NS)[0]
    word_extent = inline.find(qn(WP_NS, "extent"))
    original_width = int(word_extent.get("cx"))
    with Image.open(figure_path) as image:
        pixel_width, pixel_height = image.size
    new_height = round(original_width * pixel_height / pixel_width)
    word_extent.set("cx", str(original_width))
    word_extent.set("cy", str(new_height))

    drawing_extent = new_run.xpath(".//a:xfrm/a:ext", namespaces=NS)
    if drawing_extent:
        drawing_extent[0].set("cx", str(original_width))
        drawing_extent[0].set("cy", str(new_height))

    doc_property = new_run.xpath(".//wp:docPr", namespaces=NS)[0]
    doc_property.set("id", str(new_doc_property_id))
    doc_property.set("name", "Corrected Figure 2")
    doc_property.set(
        "descr",
        "Corrected European P-score maps for the initial and Delta waves.",
    )
    picture_property = new_run.xpath(".//pic:cNvPr", namespaces=NS)
    if picture_property:
        picture_property[0].set("name", figure_path.name)
        picture_property[0].set("descr", doc_property.get("descr"))

    insertion_index = image_paragraph.index(old_run)
    image_paragraph.remove(old_run)

    deletion = revision_element("del", revision_id, author, timestamp)
    deletion.append(old_run)
    image_paragraph.insert(insertion_index, deletion)

    insertion = revision_element("ins", revision_id + 1, author, timestamp)
    insertion.append(new_run)
    image_paragraph.insert(insertion_index + 1, insertion)
    return revision_id + 2


def enable_track_revisions(settings_root: etree._Element) -> None:
    if settings_root.find(qn(W_NS, "trackRevisions")) is None:
        settings_root.insert(0, etree.Element(qn(W_NS, "trackRevisions")))


def write_tracked_document(
    input_path: Path,
    figure_path: Path,
    output_path: Path,
    author: str,
) -> None:
    timestamp = dt.datetime.now(dt.timezone.utc).replace(microsecond=0).isoformat()
    with zipfile.ZipFile(input_path, "r") as archive:
        document_root = etree.fromstring(archive.read("word/document.xml"))
        settings_root = etree.fromstring(archive.read("word/settings.xml"))
        relationships_root = etree.fromstring(
            archive.read("word/_rels/document.xml.rels")
        )

        caption_paragraph = find_unique_paragraph(document_root, "Figure 2.")
        age_note_paragraph = find_unique_paragraph(
            document_root,
            "The age stratification is different",
        )
        body_paragraphs = document_root.xpath("//w:body/w:p", namespaces=NS)
        caption_index = body_paragraphs.index(caption_paragraph)
        image_paragraph = body_paragraphs[caption_index + 1]

        new_relationship_id = next_relationship_id(relationships_root)
        new_media_name = "media/figure_02_europe_maps_corrected.png"
        relationship = etree.SubElement(
            relationships_root,
            qn(PKG_REL_NS, "Relationship"),
        )
        relationship.set("Id", new_relationship_id)
        relationship.set(
            "Type",
            "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image",
        )
        relationship.set("Target", new_media_name)

        revision_id = next_revision_id(document_root)
        revision_id = tracked_text_replacement(
            caption_paragraph,
            CAPTION,
            revision_id,
            author,
            timestamp,
        )
        revision_id = tracked_image_replacement(
            image_paragraph,
            new_relationship_id,
            figure_path,
            revision_id,
            author,
            timestamp,
            next_doc_property_id(document_root),
        )
        tracked_text_replacement(
            age_note_paragraph,
            AGE_NOTE,
            revision_id,
            author,
            timestamp,
        )
        enable_track_revisions(settings_root)

        replacements = {
            "word/document.xml": etree.tostring(
                document_root,
                xml_declaration=True,
                encoding="UTF-8",
                standalone="yes",
            ),
            "word/settings.xml": etree.tostring(
                settings_root,
                xml_declaration=True,
                encoding="UTF-8",
                standalone="yes",
            ),
            "word/_rels/document.xml.rels": etree.tostring(
                relationships_root,
                xml_declaration=True,
                encoding="UTF-8",
                standalone="yes",
            ),
            f"word/{new_media_name}": figure_path.read_bytes(),
        }

        output_path.parent.mkdir(parents=True, exist_ok=True)
        with tempfile.NamedTemporaryFile(
            prefix="figures_updated_",
            suffix=".docx",
            dir=output_path.parent,
            delete=False,
        ) as temporary_file:
            temporary_path = Path(temporary_file.name)
        try:
            with zipfile.ZipFile(
                temporary_path,
                "w",
                compression=zipfile.ZIP_DEFLATED,
            ) as output_archive:
                for item in archive.infolist():
                    if item.filename in replacements:
                        output_archive.writestr(item, replacements.pop(item.filename))
                    else:
                        output_archive.writestr(item, archive.read(item.filename))
                for name, payload in replacements.items():
                    output_archive.writestr(name, payload)
            shutil.move(temporary_path, output_path)
        finally:
            if temporary_path.exists():
                temporary_path.unlink()


def main() -> None:
    args = parse_args()
    if not args.input.is_file():
        raise FileNotFoundError(args.input)
    if not args.figure.is_file():
        raise FileNotFoundError(args.figure)
    if args.input.resolve() == args.output.resolve():
        raise ValueError("The tracked output must not overwrite the source document.")
    write_tracked_document(args.input, args.figure, args.output, args.author)
    print(args.output)


if __name__ == "__main__":
    main()
