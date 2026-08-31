#!/usr/bin/env python3
"""Validate and materialize accepted/rejected views of tracked DOCX updates."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import tempfile
import zipfile
from pathlib import Path

from lxml import etree


W_NS = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
M_NS = "http://schemas.openxmlformats.org/officeDocument/2006/math"
NS = {"w": W_NS, "m": M_NS}
W = f"{{{W_NS}}}"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--log",
        type=Path,
        default=Path(
            "output/validation/manuscript_docx_update_20260831/"
            "replacement_log.json"
        ),
    )
    parser.add_argument(
        "--validation-root",
        type=Path,
        default=Path("output/validation/manuscript_docx_update_20260831"),
    )
    return parser.parse_args()


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def xml_bytes(root: etree._Element) -> bytes:
    return etree.tostring(
        root,
        xml_declaration=True,
        encoding="UTF-8",
        standalone="yes",
    )


def body_paragraph_texts(document: etree._Element) -> list[str]:
    paragraphs = document.xpath("/w:document/w:body/w:p", namespaces=NS)
    return ["".join(p.xpath(".//w:t/text()", namespaces=NS)) for p in paragraphs]


def unzip_xml(path: Path, member: str) -> etree._Element:
    with zipfile.ZipFile(path) as archive:
        return etree.fromstring(archive.read(member))


def unwrap(element: etree._Element) -> None:
    parent = element.getparent()
    if parent is None:
        raise RuntimeError("Cannot unwrap an XML root element")
    position = parent.index(element)
    for child in list(element):
        element.remove(child)
        parent.insert(position, child)
        position += 1
    parent.remove(element)


def paragraph_has_visible_content(paragraph: etree._Element) -> bool:
    text_nodes = paragraph.xpath(".//w:t | .//m:t", namespaces=NS)
    if any((node.text or "") for node in text_nodes):
        return True
    visible_elements = paragraph.xpath(
        ".//w:tab | .//w:br | .//w:cr | .//w:drawing | .//w:object | "
        ".//w:pict | .//w:sym | .//w:footnoteReference | "
        ".//w:endnoteReference | .//w:fldChar | .//w:instrText",
        namespaces=NS,
    )
    return bool(visible_elements)


def materialize_xml(
    payload: bytes,
    view: str,
    cleanup_empty_deleted_paragraphs: bool = False,
) -> bytes:
    root = etree.fromstring(payload)
    if view == "accept":
        deletion_paragraphs = list(root.xpath(".//w:p[.//w:del]", namespaces=NS))
        for deletion in root.xpath(".//w:del", namespaces=NS):
            deletion.getparent().remove(deletion)
        for insertion in root.xpath(".//w:ins", namespaces=NS):
            unwrap(insertion)
        for change in root.xpath(".//w:rPrChange", namespaces=NS):
            change.getparent().remove(change)
        for change in root.xpath(".//w:pPrChange", namespaces=NS):
            change.getparent().remove(change)
        if cleanup_empty_deleted_paragraphs:
            for paragraph in deletion_paragraphs:
                parent = paragraph.getparent()
                if parent is not None and not paragraph_has_visible_content(paragraph):
                    parent.remove(paragraph)
    elif view == "reject":
        for insertion in root.xpath(".//w:ins", namespaces=NS):
            insertion.getparent().remove(insertion)
        for deletion in root.xpath(".//w:del", namespaces=NS):
            for text in deletion.xpath(".//w:delText", namespaces=NS):
                text.tag = W + "t"
            for instruction in deletion.xpath(".//w:delInstrText", namespaces=NS):
                instruction.tag = W + "instrText"
            unwrap(deletion)
        for change in root.xpath(".//w:rPrChange", namespaces=NS):
            current_properties = change.getparent()
            old_properties = change.find("w:rPr", namespaces=NS)
            if old_properties is None:
                raise RuntimeError("Run-format revision lacks prior properties")
            restored = [etree.fromstring(etree.tostring(child)) for child in old_properties]
            for child in list(current_properties):
                current_properties.remove(child)
            for child in restored:
                current_properties.append(child)
        for change in root.xpath(".//w:pPrChange", namespaces=NS):
            current_properties = change.getparent()
            old_properties = change.find("w:pPr", namespaces=NS)
            if old_properties is None:
                raise RuntimeError("Paragraph-format revision lacks prior properties")
            restored = [etree.fromstring(etree.tostring(child)) for child in old_properties]
            for child in list(current_properties):
                current_properties.remove(child)
            for child in restored:
                current_properties.append(child)
    else:
        raise ValueError(f"Unknown revision view: {view}")

    for setting in root.xpath(".//w:trackRevisions", namespaces=NS):
        setting.getparent().remove(setting)
    return xml_bytes(root)


def materialize_docx(
    source: Path,
    destination: Path,
    view: str,
    cleanup_empty_deleted_paragraphs: bool = False,
) -> None:
    destination.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(
        dir=destination.parent,
        suffix=".docx",
        delete=False,
    ) as handle:
        temporary = Path(handle.name)
    try:
        with zipfile.ZipFile(source) as input_archive, zipfile.ZipFile(
            temporary, "w", zipfile.ZIP_DEFLATED
        ) as output_archive:
            for item in input_archive.infolist():
                payload = input_archive.read(item.filename)
                if item.filename.endswith(".xml"):
                    payload = materialize_xml(
                        payload,
                        view,
                        cleanup_empty_deleted_paragraphs=cleanup_empty_deleted_paragraphs,
                    )
                output_archive.writestr(item, payload)
        temporary.replace(destination)
    finally:
        temporary.unlink(missing_ok=True)


def assert_zip_integrity(path: Path) -> None:
    with zipfile.ZipFile(path) as archive:
        bad_member = archive.testzip()
    if bad_member is not None:
        raise RuntimeError(f"Corrupt ZIP member in {path}: {bad_member}")


def assert_unmodified_parts(baseline: Path, tracked: Path) -> None:
    with zipfile.ZipFile(baseline) as before, zipfile.ZipFile(tracked) as after:
        before_names = set(before.namelist())
        after_names = set(after.namelist())
        if before_names != after_names:
            raise RuntimeError(f"DOCX part inventory changed for {tracked}")
        allowed = {"word/document.xml", "word/settings.xml"}
        changed = {
            name
            for name in before_names
            if hashlib.sha256(before.read(name)).digest()
            != hashlib.sha256(after.read(name)).digest()
        }
        if not changed.issubset(allowed):
            raise RuntimeError(
                f"Unexpected DOCX parts changed for {tracked}: {sorted(changed - allowed)}"
            )


def count_math(path: Path) -> dict[str, int]:
    document = unzip_xml(path, "word/document.xml")
    return {
        "oMath": int(document.xpath("count(.//m:oMath)", namespaces=NS)),
        "oMathPara": int(document.xpath("count(.//m:oMathPara)", namespaces=NS)),
    }


def validate_tracked_xml(
    tracked: Path,
    expected_insertions: int,
    expected_deletions: int,
    expected_format_changes: int,
) -> dict:
    document = unzip_xml(tracked, "word/document.xml")
    settings = unzip_xml(tracked, "word/settings.xml")
    insertions = document.xpath(".//w:ins", namespaces=NS)
    deletions = document.xpath(".//w:del", namespaces=NS)
    format_changes = document.xpath(".//w:rPrChange", namespaces=NS)
    if len(insertions) != expected_insertions:
        raise RuntimeError(
            f"Expected {expected_insertions} insertions in {tracked}, found {len(insertions)}"
        )
    if len(deletions) != expected_deletions:
        raise RuntimeError(
            f"Expected {expected_deletions} deletions in {tracked}, found {len(deletions)}"
        )
    if len(format_changes) != expected_format_changes:
        raise RuntimeError(
            f"Expected {expected_format_changes} format changes in {tracked}, "
            f"found {len(format_changes)}"
        )
    nested = document.xpath(
        ".//w:ins//w:ins | .//w:ins//w:del | .//w:del//w:ins | .//w:del//w:del",
        namespaces=NS,
    )
    if nested:
        raise RuntimeError(f"Nested revisions found in {tracked}")
    revisions = insertions + deletions + format_changes
    identifiers = [revision.get(W + "id") for revision in revisions]
    if None in identifiers or len(set(identifiers)) != len(identifiers):
        raise RuntimeError(f"Revision IDs are missing or duplicated in {tracked}")
    if any(not identifier.isdigit() for identifier in identifiers):
        raise RuntimeError(f"Non-integer revision ID found in {tracked}")
    authors = sorted({revision.get(W + "author") for revision in revisions})
    dates = sorted({revision.get(W + "date") for revision in revisions})
    if authors != ["Codex"]:
        raise RuntimeError(f"Unexpected revision authors in {tracked}: {authors}")
    if dates != ["2026-08-31T06:15:00Z"]:
        raise RuntimeError(f"Unexpected revision dates in {tracked}: {dates}")
    if not settings.xpath(".//w:trackRevisions", namespaces=NS):
        raise RuntimeError(f"Track Changes is not enabled in {tracked}")
    return {
        "insertions": len(insertions),
        "deletions": len(deletions),
        "format_changes": len(format_changes),
        "author": authors[0],
        "date_utc": dates[0],
    }


def assert_materialized_view(
    baseline: Path,
    accepted: Path,
    rejected: Path,
    records: list[dict],
) -> tuple[list[str], list[str], list[str]]:
    baseline_document = unzip_xml(baseline, "word/document.xml")
    accepted_document = unzip_xml(accepted, "word/document.xml")
    rejected_document = unzip_xml(rejected, "word/document.xml")
    baseline_texts = body_paragraph_texts(baseline_document)
    accepted_texts = body_paragraph_texts(accepted_document)
    rejected_texts = body_paragraph_texts(rejected_document)
    if rejected_texts != baseline_texts:
        mismatches = [
            index
            for index, (before, after) in enumerate(zip(baseline_texts, rejected_texts))
            if before != after
        ]
        raise RuntimeError(
            f"Rejected view does not reproduce baseline; mismatches: {mismatches[:10]}"
        )
    for document in (baseline_document, rejected_document):
        for proofing_marker in document.xpath(".//w:proofErr", namespaces=NS):
            proofing_marker.getparent().remove(proofing_marker)
    baseline_c14n = etree.tostring(baseline_document, method="c14n")
    rejected_c14n = etree.tostring(rejected_document, method="c14n")
    if rejected_c14n != baseline_c14n:
        raise RuntimeError("Rejected document XML does not exactly reproduce baseline formatting")
    expected = list(baseline_texts)
    for record in records:
        expected[int(record["paragraph_index"])] = record["new_text"]
    if accepted_texts != expected:
        mismatches = [
            index
            for index, (wanted, observed) in enumerate(zip(expected, accepted_texts))
            if wanted != observed
        ]
        raise RuntimeError(
            f"Accepted view does not match manifest; mismatches: {mismatches[:10]}"
        )
    for path in (accepted, rejected):
        document = unzip_xml(path, "word/document.xml")
        settings = unzip_xml(path, "word/settings.xml")
        if document.xpath(".//w:ins | .//w:del", namespaces=NS):
            raise RuntimeError(f"Materialized view still contains revisions: {path}")
        if document.xpath(".//w:rPrChange | .//w:pPrChange", namespaces=NS):
            raise RuntimeError(f"Materialized view still contains format revisions: {path}")
        if settings.xpath(".//w:trackRevisions", namespaces=NS):
            raise RuntimeError(f"Materialized view still enables tracking: {path}")
    return baseline_texts, accepted_texts, rejected_texts


def normalize_text(paragraphs: list[str]) -> str:
    return "\n".join(paragraphs)


def assert_manuscript_claims(text: str, paragraphs: list[str]) -> int:
    required = [
        "weekly, monthly, or quarterly all-cause mortality",
        "10 Canadian provinces",
        "January 5, 1981–August 28, 2023",
        "10 provinces and three territories",
        "Arkansas, California, New York, and Texas fell between the fixed thresholds",
        "Portugal, and Spain",
        "a second-order integrated Wiener process (IWP2)",
        "periods of 12, 6, 4, and 3 months",
        "Europe and England-and-Wales fits used BayesGP with five-point adaptive Gauss-Hermite quadrature specified explicitly",
        "Ireland used the equivalent TMB implementation with five-point quadrature",
        "US and Canadian fits used BayesGP with its four-point quadrature default",
        "posterior predictive counts were drawn directly at the quarterly level",
        "a controlled sensitivity comparison",
        "nine provinces passed the sex-stratified fitting gate",
        "13.0% versus 17.9% and 21.2% versus 27.2%",
        "+9.4 percentage points for low-vaccination European ages 40–79",
        "+4.7 points for low-vaccination US ages 65–84",
        "We did not conduct a continuous-coverage sensitivity analysis.",
        "do not establish individual-level effects",
        "Large fitted-model and frozen reporting artifacts required for exact reproduction will be archived on Zenodo",
        "Region-level comparisons describe these patterns but cannot attribute them to a single cause.",
        "EClinicalMedicine. 2026;98:104072",
    ]
    prohibited = [
        "Bayesian hierarchical models",
        "third-order integrated Wiener processes",
        "periods of one year, six months, three months, and 1.5 months",
        "P-scores >40% in the ≥80 age group",
        "males showed 5–15 percentage point higher P-scores",
        "New York surpassed 200%",
        "interactive web application",
        "Sensitivity analyses of cause-specific mortality",
        "sensitivity analyses using continuous coverage metrics",
        "Canada as a homogeneous high-vaccination context",
        "thereby establishing excess mortality as the gold standard",
        "age pattern became increasingly contingent on vaccination coverage",
        "10.1101/2025.07.15.25331003",
    ]
    missing = [phrase for phrase in required if phrase not in text]
    present = [phrase for phrase in prohibited if phrase in text]
    if missing:
        raise RuntimeError(f"Required manuscript statements missing: {missing}")
    if present:
        raise RuntimeError(f"Superseded manuscript statements remain: {present}")

    try:
        abstract_start = paragraphs.index("Abstract")
    except ValueError as error:
        raise RuntimeError("Accepted manuscript lacks the Abstract heading") from error
    abstract_paragraphs = paragraphs[abstract_start + 1 : abstract_start + 5]
    abstract_words = len(re.findall(r"\S+", " ".join(abstract_paragraphs)))
    if abstract_words > 250:
        raise RuntimeError(f"Abstract exceeds 250 words: {abstract_words}")
    return abstract_words


def assert_appendix_claims(text: str) -> None:
    required = [
        "fitted independently to each region-by-stratum series rather than hierarchically pooled",
        "second-order integrated Wiener process",
        "four harmonics for weekly and monthly analyses and one annual harmonic",
        "Europe and England-and-Wales fits used BayesGP with five-point adaptive Gauss-Hermite quadrature specified explicitly",
        "Ireland used the equivalent TMB implementation with five-point quadrature",
        "US and Canadian fits used BayesGP with its four-point quadrature default",
        "no population-denominator offset is used",
        "posterior predictive mortality is drawn directly as quarterly Poisson counts",
        "Figure 4 uses ages 40–59 and 60–79 in both Europe and the US",
        "US 0–84 Figure 5 estimand combines ages 0–44, 45–64, and 65–84",
        "female-minus-male P-score difference",
    ]
    prohibited = [
        "third-order integrated Wiener process",
        "The models are fitted in a hierarchical framework",
        "40–64 combined-age estimand",
    ]
    missing = [phrase for phrase in required if phrase not in text]
    present = [phrase for phrase in prohibited if phrase in text]
    if missing:
        raise RuntimeError(f"Required appendix statements missing: {missing}")
    if present:
        raise RuntimeError(f"Superseded appendix statements remain: {present}")


def main() -> None:
    args = parse_args()
    project_root = Path.cwd().resolve()
    log_path = args.log if args.log.is_absolute() else project_root / args.log
    validation_root = (
        args.validation_root
        if args.validation_root.is_absolute()
        else project_root / args.validation_root
    )
    summaries = json.loads(log_path.read_text())
    report: dict[str, dict] = {}

    for summary in summaries:
        document_id = summary["document_id"]
        baseline = Path(summary["baseline"])
        tracked = Path(summary["output"])
        accepted = validation_root / "accepted" / f"{document_id}_accepted.docx"
        rejected = validation_root / "rejected" / f"{document_id}_rejected.docx"
        expected_deletions = len(summary["records"])
        expected_insertions = sum(bool(record["new_text"]) for record in summary["records"])
        expected_format_changes = int(summary.get("format_change_count", 0))

        for path in (baseline, tracked):
            assert_zip_integrity(path)
        if sha256_file(baseline) != summary["baseline_sha256"]:
            raise RuntimeError(f"Baseline hash mismatch for {document_id}")
        if sha256_file(tracked) != summary["output_sha256"]:
            raise RuntimeError(f"Tracked output hash mismatch for {document_id}")
        assert_unmodified_parts(baseline, tracked)
        revision_summary = validate_tracked_xml(
            tracked,
            expected_insertions,
            expected_deletions,
            expected_format_changes,
        )
        if count_math(baseline) != count_math(tracked):
            raise RuntimeError(f"Equation inventory changed for {document_id}")

        materialize_docx(tracked, accepted, "accept")
        materialize_docx(tracked, rejected, "reject")
        for path in (accepted, rejected):
            assert_zip_integrity(path)
        baseline_texts, accepted_texts, _ = assert_materialized_view(
            baseline,
            accepted,
            rejected,
            summary["records"],
        )
        accepted_text = normalize_text(accepted_texts)
        rejected_text = normalize_text(baseline_texts)
        text_directory = validation_root / "text"
        text_directory.mkdir(parents=True, exist_ok=True)
        (text_directory / f"{document_id}_accepted.txt").write_text(
            accepted_text + "\n",
            encoding="utf-8",
        )
        (text_directory / f"{document_id}_rejected.txt").write_text(
            rejected_text + "\n",
            encoding="utf-8",
        )

        abstract_words = None
        if document_id == "manuscript":
            abstract_words = assert_manuscript_claims(accepted_text, accepted_texts)
            accepted_document = unzip_xml(accepted, "word/document.xml")
            if accepted_document.xpath(".//w:highlight | .//w:shd", namespaces=NS):
                raise RuntimeError("Accepted manuscript retains drafting highlights")
        elif document_id == "appendix":
            assert_appendix_claims(accepted_text)

        report[document_id] = {
            "baseline": str(baseline),
            "baseline_sha256": sha256_file(baseline),
            "tracked": str(tracked),
            "tracked_sha256": sha256_file(tracked),
            "accepted": str(accepted),
            "accepted_sha256": sha256_file(accepted),
            "rejected": str(rejected),
            "rejected_sha256": sha256_file(rejected),
            "replacements": len(summary["records"]),
            "revisions": revision_summary,
            "math_inventory": count_math(tracked),
            "abstract_words": abstract_words,
        }

    report_path = validation_root / "structural_validation.json"
    report_path.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(f"[OK] tracked DOCX structural validation passed: {report_path}")


if __name__ == "__main__":
    main()
