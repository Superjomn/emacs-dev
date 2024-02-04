#!/usr/bin/env python
import argparse
from datetime import datetime
import click
from dataclasses import dataclass
from typing import List, Tuple


@dataclass
class Subtitle:
    start_time: str
    end_time: str
    text: str


def parse_subtitles(subtitles_file: str):
    subtitles = []
    with open(subtitles_file, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    for i in range(0, len(lines), 4):
        start_time, end_time = lines[i+1].split(" --> ")
        text = lines[i+2].strip()
        subtitles.append(Subtitle(start_time.strip(), end_time.strip(), text))

    return subtitles


def generate_subtitles_clusters(subtitles: list):
    '''
    Generate clusters of subtitles based on the text.
    '''
    end_punctuation = ['.', '!', '?']

    clusters: List[Tuple[int, int]] = []
    start = 1

    for i in range(1, len(subtitles)+1):
        if subtitles[i-1].text[-1] in end_punctuation:
            clusters.append((start, i))
            start = i + 1
    return clusters


def merge_subtitle_cluster(subtitles: List[Subtitle], cluster: Tuple[int, int]):
    if cluster[0] == cluster[1]:
        return subtitles[cluster[0]-1]
    start_time = subtitles[cluster[0]-1].start_time
    end_time = subtitles[cluster[1]-1].end_time
    text = ' '.join([sub.text for sub in subtitles[cluster[0]-1:cluster[1]]])
    return Subtitle(start_time, end_time, text)


def merge_subtitles(subtitles: List[Subtitle], clusters: List[Tuple[int, int]]):
    merged_subtitles = []
    for cluster in clusters:
        merged_subtitles.append(merge_subtitle_cluster(subtitles, cluster))
    return merged_subtitles


@click.command()
@click.option('--task', type=click.Choice(['cluster', 'merge']), default='merge')
@click.option('--subtitle_file', type=click.Path(exists=True), required=True)
@click.option('--cluster_file', type=click.Path(exists=True))
@click.option('--output_file', type=click.Path())
def main(
        task: str,
        subtitle_file: str, cluster_file: str, output_file: str):
    if task == 'cluster':
        subtitles = parse_subtitles(subtitle_file)
        clusters = generate_subtitles_clusters(subtitles)

        if not output_file:
            for cluster in clusters:
                print(cluster)
            return
        with open(output_file, 'w') as f:
            for cluster in clusters:
                f.write(f"{cluster[0]} {cluster[1]}\n")
        return

    if task == 'merge':
        subtitles = parse_subtitles(subtitle_file)
        clusters = []
        with open(cluster_file, 'r') as f:
            for line in f.readlines():
                start, end = line.strip().split()
                clusters.append((int(start), int(end)))

        merged_subtitles = merge_subtitles(subtitles, clusters)

        with open(output_file, 'w', encoding='utf-8') as f:
            for i, sub in enumerate(merged_subtitles):
                f.write(
                    f"{i+1}\n{sub.start_time} --> {sub.end_time}\n{sub.text}\n\n")


if __name__ == '__main__':
    main()
