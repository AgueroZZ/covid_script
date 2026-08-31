#!/usr/bin/env python3
"""Apply deterministic paragraph-level Word tracked changes from a YAML manifest."""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
import re
import subprocess
import tempfile
import zipfile
from collections import Counter
from pathlib import Path

from lxml import etree


W_NS = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
XML_NS = "http://www.w3.org/XML/1998/namespace"
NS = {"w": W_NS}
W = f"{{{W_NS}}}"
XML_SPACE = f"{{{XML_NS}}}space"
MARKUP = re.compile(r"\[\[(bold|italic|sup):(.+?)\]\]")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--manifest",
        type=Path,
        default=Path("config/manuscript_tracked_replacements.yml"),
    )
    parser.add_argument("--project-root", type=Path, default=Path.cwd())
    return parser.parse_args()


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def read_yaml_with_r(path: Path) -> dict:
    """Parse YAML through the project's existing R yaml/jsonlite packages."""
    with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as handle:
        json_path = Path(handle.name)
    expression = (
        "x <- yaml::read_yaml(commandArgs(trailingOnly=TRUE)[1]); "
        "jsonlite::write_json(x, commandArgs(trailingOnly=TRUE)[2], "
        "auto_unbox=TRUE, pretty=TRUE, null='null')"
    )
    try:
        subprocess.run(
            ["Rscript", "--vanilla", "-e", expression, str(path), str(json_path)],
            check=True,
        )
        return json.loads(json_path.read_text())
    finally:
        json_path.unlink(missing_ok=True)


def xml_bytes(root: etree._Element) -> bytes:
    return etree.tostring(
        root,
        xml_declaration=True,
        encoding="UTF-8",
        standalone="yes",
    )


def paragraph_text(paragraph: etree._Element) -> str:
    return "".join(paragraph.xpath(".//w:t/text()", namespaces=NS))


def next_revision_id(document: etree._Element) -> int:
    identifiers: list[int] = []
    for element in document.xpath(".//*[@w:id]", namespaces=NS):
        try:
            identifiers.append(int(element.get(W + "id")))
        except (TypeError, ValueError):
            continue
    return max(identifiers, default=0) + 1


def enable_tracking(settings: etree._Element) -> None:
    if settings.find("w:trackRevisions", namespaces=NS) is None:
        settings.insert(0, etree.Element(W + "trackRevisions"))


def ordinary_run_properties(paragraph: etree._Element) -> etree._Element | None:
    candidates: list[tuple[int, etree._Element | None]] = []
    for run in paragraph.xpath(".//w:r", namespaces=NS):
        text = "".join(run.xpath(".//w:t/text()", namespaces=NS))
        if not text:
            continue
        properties = run.find("w:rPr", namespaces=NS)
        special = False
        if properties is not None:
            special = any(
                properties.find(f"w:{tag}", namespaces=NS) is not None
                for tag in ("b", "i", "vertAlign", "highlight")
            )
        if not special:
            candidates.append((len(text), properties))
    if not candidates:
        runs = paragraph.xpath(".//w:r", namespaces=NS)
        if not runs:
            return None
        properties = runs[0].find("w:rPr", namespaces=NS)
        return copy.deepcopy(properties) if properties is not None else None
    properties = max(candidates, key=lambda item: item[0])[1]
    return copy.deepcopy(properties) if properties is not None else None


def clone_run_as_deleted(run: etree._Element) -> etree._Element | None:
    cloned = copy.deepcopy(run)
    for text in cloned.xpath(".//w:t", namespaces=NS):
        text.tag = W + "delText"
        value = text.text or ""
        if value and (value[0].isspace() or value[-1].isspace()):
            text.set(XML_SPACE, "preserve")
    for instruction in cloned.xpath(".//w:instrText", namespaces=NS):
        instruction.tag = W + "delInstrText"
    has_content = any(child.tag != W + "rPr" for child in cloned)
    return cloned if has_content else None


def make_revision(tag: str, revision_id: int, author: str, date_utc: str) -> etree._Element:
    element = etree.Element(W + tag)
    element.set(W + "id", str(revision_id))
    element.set(W + "author", author)
    element.set(W + "date", date_utc)
    return element


def set_toggle(properties: etree._Element, tag: str) -> None:
    existing = properties.find(f"w:{tag}", namespaces=NS)
    if existing is None:
        etree.SubElement(properties, W + tag)


def make_inserted_run(
    text: str,
    style: str | None,
    base_properties: etree._Element | None,
) -> etree._Element:
    run = etree.Element(W + "r")
    properties = (
        copy.deepcopy(base_properties)
        if base_properties is not None
        else etree.Element(W + "rPr")
    )
    for tag in ("highlight", "shd", "color"):
        inherited = properties.find(f"w:{tag}", namespaces=NS)
        if inherited is not None:
            properties.remove(inherited)
    if style == "bold":
        set_toggle(properties, "b")
    elif style == "italic":
        set_toggle(properties, "i")
    elif style == "sup":
        vertical = properties.find("w:vertAlign", namespaces=NS)
        if vertical is None:
            vertical = etree.SubElement(properties, W + "vertAlign")
        vertical.set(W + "val", "superscript")
    if len(properties):
        run.append(properties)
    text_node = etree.SubElement(run, W + "t")
    text_node.text = text
    if text and (text[0].isspace() or text[-1].isspace()):
        text_node.set(XML_SPACE, "preserve")
    return run


def parse_markup(markup: str) -> list[tuple[str, str | None]]:
    segments: list[tuple[str, str | None]] = []
    position = 0
    for match in MARKUP.finditer(markup):
        if match.start() > position:
            segments.append((markup[position : match.start()], None))
        segments.append((match.group(2), match.group(1)))
        position = match.end()
    if position < len(markup):
        segments.append((markup[position:], None))
    if "[[" in "".join(text for text, _ in segments):
        raise RuntimeError(f"Unparsed markup remains in {markup!r}")
    return [(text, style) for text, style in segments if text]


def replacement_text(markup: str) -> str:
    return "".join(text for text, _ in parse_markup(markup))


def replace_paragraph(
    paragraph: etree._Element,
    markup: str,
    revision_id: int,
    author: str,
    date_utc: str,
) -> tuple[int, int, int]:
    if paragraph.xpath(".//w:ins | .//w:del", namespaces=NS):
        raise RuntimeError("Accepted-view baseline unexpectedly contains revisions")

    base_properties = ordinary_run_properties(paragraph)
    original_runs = paragraph.xpath(".//w:r", namespaces=NS)
    deletion = make_revision("del", revision_id, author, date_utc)
    revision_id += 1
    deleted_runs = 0
    for run in original_runs:
        deleted_run = clone_run_as_deleted(run)
        if deleted_run is not None:
            deletion.append(deleted_run)
            deleted_runs += 1

    preserved = [
        copy.deepcopy(child)
        for child in paragraph
        if child.tag in {W + "pPr", W + "bookmarkStart", W + "bookmarkEnd"}
    ]
    for child in list(paragraph):
        paragraph.remove(child)
    for child in preserved:
        paragraph.append(child)
    if deleted_runs:
        paragraph.append(deletion)

    inserted_runs = 0
    if markup:
        insertion = make_revision("ins", revision_id, author, date_utc)
        revision_id += 1
        for text, style in parse_markup(markup):
            insertion.append(make_inserted_run(text, style, base_properties))
            inserted_runs += 1
        paragraph.append(insertion)
    return revision_id, deleted_runs, inserted_runs


def remove_highlight_with_tracked_format_change(
    paragraph: etree._Element,
    revision_id: int,
    author: str,
    date_utc: str,
) -> tuple[int, int]:
    """Remove run highlighting while retaining the prior formatting as a revision."""
    changed_runs = 0
    for properties in paragraph.xpath(".//w:rPr", namespaces=NS):
        if properties.find("w:rPrChange", namespaces=NS) is not None:
            raise RuntimeError("Baseline unexpectedly contains a run-format revision")
        highlighted = properties.find("w:highlight", namespaces=NS)
        shading = properties.find("w:shd", namespaces=NS)
        if highlighted is None and shading is None:
            continue
        old_properties = copy.deepcopy(properties)
        if highlighted is not None:
            properties.remove(highlighted)
        if shading is not None:
            properties.remove(shading)
        change = make_revision("rPrChange", revision_id, author, date_utc)
        revision_id += 1
        change.append(old_properties)
        properties.append(change)
        changed_runs += 1
    if changed_runs == 0:
        raise RuntimeError("Requested highlight removal found no highlighted run")
    return revision_id, changed_runs


def write_docx(
    baseline: Path,
    output: Path,
    document_xml: bytes,
    settings_xml: bytes,
) -> None:
    output.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(
        dir=output.parent,
        suffix=".docx",
        delete=False,
    ) as handle:
        temporary = Path(handle.name)
    try:
        with zipfile.ZipFile(baseline) as source, zipfile.ZipFile(
            temporary, "w", zipfile.ZIP_DEFLATED
        ) as destination:
            for item in source.infolist():
                if item.filename == "word/document.xml":
                    payload = document_xml
                elif item.filename == "word/settings.xml":
                    payload = settings_xml
                else:
                    payload = source.read(item.filename)
                destination.writestr(item, payload)
        temporary.replace(output)
    finally:
        if temporary.exists():
            temporary.unlink()


def apply_document(
    project_root: Path,
    document_id: str,
    specification: dict,
    author: str,
    date_utc: str,
) -> dict:
    baseline = project_root / specification["baseline"]
    output = project_root / specification["output"]
    observed_hash = sha256_file(baseline)
    if observed_hash != specification["baseline_sha256"]:
        raise RuntimeError(
            f"{document_id} baseline hash changed: {observed_hash}"
        )

    with zipfile.ZipFile(baseline) as archive:
        document = etree.fromstring(archive.read("word/document.xml"))
        settings = etree.fromstring(archive.read("word/settings.xml"))
    enable_tracking(settings)
    paragraphs = document.xpath("/w:document/w:body/w:p", namespaces=NS)
    revision_id = next_revision_id(document)
    seen_indices: set[int] = set()
    records: list[dict] = []
    format_records: list[dict] = []

    for replacement in specification["replacements"]:
        index = int(replacement["paragraph_index"])
        if index in seen_indices:
            raise RuntimeError(f"Duplicate paragraph index for {document_id}: {index}")
        seen_indices.add(index)
        if index < 0 or index >= len(paragraphs):
            raise RuntimeError(f"Paragraph index out of range: {document_id}:{index}")
        paragraph = paragraphs[index]
        old_text = paragraph_text(paragraph)
        new_markup = replacement.get("new_markup", "") or ""
        new_text = replacement_text(new_markup)
        if old_text == new_text:
            raise RuntimeError(f"Replacement makes no change: {replacement['id']}")
        starting_revision_id = revision_id
        revision_id, deleted_runs, inserted_runs = replace_paragraph(
            paragraph,
            new_markup,
            revision_id,
            author,
            date_utc,
        )
        records.append(
            {
                "id": replacement["id"],
                "paragraph_index": index,
                "old_text": old_text,
                "new_text": new_text,
                "old_text_sha256": hashlib.sha256(old_text.encode("utf-8")).hexdigest(),
                "new_text_sha256": hashlib.sha256(new_text.encode("utf-8")).hexdigest(),
                "starting_revision_id": starting_revision_id,
                "ending_revision_id": revision_id - 1,
                "deleted_runs": deleted_runs,
                "inserted_runs": inserted_runs,
            }
        )

    for format_change in specification.get("format_changes", []):
        index = int(format_change["paragraph_index"])
        if index < 0 or index >= len(paragraphs):
            raise RuntimeError(f"Paragraph index out of range: {document_id}:{index}")
        if not format_change.get("remove_highlight", False):
            raise RuntimeError(
                f"Unsupported format change: {format_change['id']}"
            )
        starting_revision_id = revision_id
        revision_id, changed_runs = remove_highlight_with_tracked_format_change(
            paragraphs[index],
            revision_id,
            author,
            date_utc,
        )
        format_records.append(
            {
                "id": format_change["id"],
                "paragraph_index": index,
                "change": "remove_highlight",
                "changed_runs": changed_runs,
                "starting_revision_id": starting_revision_id,
                "ending_revision_id": revision_id - 1,
            }
        )

    write_docx(baseline, output, xml_bytes(document), xml_bytes(settings))
    return {
        "document_id": document_id,
        "baseline": str(baseline),
        "baseline_sha256": observed_hash,
        "output": str(output),
        "output_sha256": sha256_file(output),
        "replacement_count": len(records),
        "records": records,
        "format_change_count": len(format_records),
        "format_records": format_records,
    }


def main() -> None:
    args = parse_args()
    project_root = args.project_root.resolve()
    manifest_path = (
        args.manifest
        if args.manifest.is_absolute()
        else project_root / args.manifest
    )
    manifest = read_yaml_with_r(manifest_path)
    summaries = []
    for document_id, specification in manifest["documents"].items():
        summaries.append(
            apply_document(
                project_root,
                document_id,
                specification,
                manifest["author"],
                manifest["date_utc"],
            )
        )

    log_path = project_root / "artifacts/validation/manuscript_docx_update_20260831/replacement_log.json"
    log_path.parent.mkdir(parents=True, exist_ok=True)
    log_path.write_text(json.dumps(summaries, indent=2, ensure_ascii=False) + "\n")
    counts = Counter(
        record["document_id"]
        for record in summaries
    )
    print(
        "[OK] wrote tracked documents: "
        + ", ".join(
            f"{summary['document_id']} ({summary['replacement_count']} paragraphs, "
            f"{summary['format_change_count']} format changes)"
            for summary in summaries
        )
    )


if __name__ == "__main__":
    main()
