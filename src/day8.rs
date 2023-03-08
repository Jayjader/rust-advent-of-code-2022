use std::collections::HashSet;

use crate::{Part, Solution};

fn parse_tree_heights(input: &str) -> Vec<Vec<usize>> {
    input
        .split_terminator('\n')
        .map(|line| line.split("").flat_map(|c| c.parse::<usize>()).collect())
        .collect()
}

/// solves the problem for day 8
pub fn day8(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> usize {
        let tree_heights: Vec<Vec<usize>> = parse_tree_heights(input);
        let square_size = tree_heights.len();
        assert_eq!(square_size, tree_heights[0].len()); // make sure we're really a square
        let mut visible = HashSet::new();
        for (y, line) in tree_heights.iter().enumerate() {
            let visible_from_the_left = line.iter().enumerate().fold(
                Vec::<(usize, usize)>::with_capacity(square_size),
                |mut previous, (index, height)| {
                    if index == 0 || height > previous.last().map(|(_, h)| h).unwrap_or(&0usize) {
                        previous.push((index, *height));
                    }
                    previous
                },
            );
            let visible_from_the_right = line.iter().rev().enumerate().fold(
                Vec::<(usize, usize)>::with_capacity(square_size),
                |mut previous, (index, height)| {
                    if index == 0 || height > previous.last().map(|(_, h)| h).unwrap_or(&0usize) {
                        previous.push((square_size - index - 1, *height));
                    }
                    previous
                },
            );
            let set_from_the_left: HashSet<_> =
                HashSet::from_iter(visible_from_the_left.into_iter());
            let set_from_the_right = HashSet::from_iter(visible_from_the_right.into_iter());
            let visible_horizontally = set_from_the_left.union(&set_from_the_right);
            for item in visible_horizontally {
                let (x, height) = item;
                visible.insert(((*x, y), *height));
            }
        }
        for x in 0..square_size {
            let visible_from_the_top = tree_heights.iter().map(|line| line[x]).enumerate().fold(
                Vec::<(usize, usize)>::with_capacity(square_size),
                |mut previous, (index, height)| {
                    if index == 0 || height > *(previous.last().map(|(_, h)| h).unwrap_or(&0usize))
                    {
                        previous.push((index, height));
                    }
                    previous
                },
            );
            let visible_from_the_bottom = tree_heights
                .iter()
                .map(|line| line[x])
                .rev()
                .enumerate()
                .fold(
                    Vec::<(usize, usize)>::with_capacity(square_size),
                    |mut previous, (index, height)| {
                        if index == 0
                            || height > *(previous.last().map(|(_, h)| h).unwrap_or(&0usize))
                        {
                            previous.push((square_size - index - 1, height));
                        }
                        previous
                    },
                );
            let set_from_the_top: HashSet<_> = HashSet::from_iter(visible_from_the_top.into_iter());
            let set_from_the_bottom: HashSet<_> =
                HashSet::from_iter(visible_from_the_bottom.into_iter());
            let visible_vertically = set_from_the_top.union(&set_from_the_bottom);
            for item in visible_vertically {
                let (y, height) = item;
                visible.insert(((x, *y), *height));
            }
        }
        visible.len()
    }

    fn part2(input: &str) -> usize {
        let tree_heights = parse_tree_heights(input);
        let square_size = tree_heights.len();
        let mut max = 0;
        for y in 0..square_size {
            for x in 0..square_size {
                let candidate_tree_height = tree_heights[y][x];
                let visible_to_the_left = if x == 0 {
                    0
                } else {
                    tree_heights[y][0..x]
                        .iter()
                        .rev()
                        .position(|height| *height >= candidate_tree_height)
                        .unwrap_or(x - 1)
                        + 1
                };
                let visible_to_the_right = if x == square_size - 1 {
                    0
                } else {
                    tree_heights[y][x + 1..]
                        .iter()
                        .position(|height| *height >= candidate_tree_height)
                        .unwrap_or(square_size - x - 2)
                        + 1
                };
                let visible_to_the_top = if y == 0 {
                    0
                } else {
                    tree_heights
                        .iter()
                        .map(|line| line[x])
                        .take(y)
                        .rev()
                        .position(|height| height >= candidate_tree_height)
                        .unwrap_or(y - 1)
                        + 1
                };
                let visible_to_the_bottom = if y == square_size - 1 {
                    0
                } else {
                    tree_heights
                        .iter()
                        .map(|line| line[x])
                        .skip(y + 1)
                        .position(|height| height >= candidate_tree_height)
                        .unwrap_or(square_size - y - 2)
                        + 1
                };
                let scenic_score = visible_to_the_left
                    * visible_to_the_right
                    * visible_to_the_top
                    * visible_to_the_bottom;
                if scenic_score > max {
                    max = scenic_score;
                }
            }
        }
        max
    }

    match part {
        Part::One => Solution::USize(part1(input)),
        Part::Two => Solution::USize(part2(input)),
    }
}
