#!/usr/bin/env python3
"""Create clean accepted-view baselines from the tracked manuscript masters."""

from __future__ import annotations

import argparse
import json
import sys
import zipfile
from pathlib import Path

from lxml import etree

PROJECT_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(PROJECT_ROOT))

from scripts.validation.validate_manuscript_tracked_update import (  # noqa: E402
    count_math,
    materialize_docx,
    sha256_file,
)


W_NS = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
NS = {"w": W_NS}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--manuscript-source",
        type=Path,
        default=Path(
            "/Users/ziangzhang/Desktop/covid_mortality/covid_agents/"
            "manuscript_July30_2026_wave_definitions_tracked.docx"
        ),
    )
    parser.add_argument(
        "--appendix-source",
        type=Path,
        default=Path(
            "/Users/ziangzhang/Desktop/covid_mortality/covid_agents/"
            "appendices_tracked.docx"
        ),
    )
    parser.add_argument(
        "--output-root",
        type=Path,
        default=Path(
            "output/validation/manuscript_docx_update_20260831/baseline"
        ),
    )
    return parser.parse_args()


def count_revisions(path: Path) -> dict[str, int]:
    tags = ("ins", "del", "rPrChange", "pPrChange")
    counts = {tag: 0 for tag in tags}
    with zipfile.ZipFile(path) as archive:
        for member in archive.namelist():
            if not member.endswith(".xml"):
                continue
            try:
                root = etree.fromstring(archive.read(member))
            except etree.XMLSyntaxError:
                continue
            for tag in tags:
                counts[tag] += len(root.xpath(f".//w:{tag}", namespaces=NS))
    return counts


def main() -> None:
    args = parse_args()
    expected_hashes = {
        "manuscript": "2527627de3a2b6f93db28e3b6ac6062cafeb7b0698e142a72f053c0ae0ed142d",
        "appendix": "3aa245f0d519e77c0a8492eaaa2f2836922a0072289068b92bfb13d187f8f0d0",
    }
    sources = {
        "manuscript": args.manuscript_source.resolve(),
        "appendix": args.appendix_source.resolve(),
    }
    destinations = {
        "manuscript": args.output_root.resolve() / "manuscript_accepted_baseline.docx",
        "appendix": args.output_root.resolve() / "appendix_accepted_baseline.docx",
    }
    records = {}
    for document_id, source in sources.items():
        observed_hash = sha256_file(source)
        if observed_hash != expected_hashes[document_id]:
            raise RuntimeError(
                f"Tracked {document_id} source hash changed: {observed_hash}"
            )
        destination = destinations[document_id]
        materialize_docx(
            source,
            destination,
            "accept",
            cleanup_empty_deleted_paragraphs=True,
        )
        remaining = count_revisions(destination)
        if any(remaining.values()):
            raise RuntimeError(
                f"Accepted {document_id} baseline retains revisions: {remaining}"
            )
        records[document_id] = {
            "source": str(source),
            "source_sha256": observed_hash,
            "baseline": str(destination),
            "baseline_sha256": sha256_file(destination),
            "math_inventory": count_math(destination),
            "remaining_revisions": remaining,
        }

    provenance = args.output_root.resolve() / "baseline_provenance.json"
    provenance.write_text(json.dumps(records, indent=2) + "\n", encoding="utf-8")
    print(f"[OK] wrote clean accepted-view baselines: {provenance}")


if __name__ == "__main__":
    main()
