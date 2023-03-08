use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

use crate::{Part, Solution};

type Position = (usize, usize);

#[derive(Debug, PartialEq, Copy, Clone)]
enum Solid {
    Rock,
    Sand,
}

fn parse_input_paths(input: &str) -> (HashMap<Position, Solid>, (Position, Position)) {
    input
        .lines()
        .map(|path| {
            path.split(" -> ")
                .map(|coords| {
                    let (x, y) = coords.split_once(',').unwrap();
                    (x.parse::<usize>().unwrap(), y.parse::<usize>().unwrap())
                })
                .collect::<Vec<_>>()
        })
        .fold(
            (HashMap::new(), ((500, 0), (500, 0))),
            |(mut map, ((mut min_x, mut min_y), (mut max_x, mut max_y))), path| {
                for pair in path.windows(2) {
                    let start = pair.first().unwrap();
                    let end = pair.last().unwrap();
                    if start.0 == end.0 {
                        // vertical segment
                        let diff = end.1 as isize - start.1 as isize;
                        let dir = diff.signum();
                        for dy in 0..=diff.abs() {
                            let y = (start.1 as isize + dy * dir) as usize;
                            map.insert((start.0, y), Solid::Rock);
                        }
                    } else {
                        // horizontal segment
                        let dir = (end.0 as isize - start.0 as isize).signum();
                        for dx in 0..=(end.0 as isize - start.0 as isize).abs() {
                            let x = (start.0 as isize + dx * dir) as usize;
                            map.insert((x, start.1), Solid::Rock);
                        }
                    }
                    max_y = max_y.max(start.1.max(end.1));
                    min_y = min_y.min(start.1.max(end.1));
                    max_x = max_x.max(start.0.max(end.0));
                    min_x = min_x.min(start.0.max(end.0));
                }
                (map, ((min_x, min_y), (max_x, max_y)))
            },
        )
}

fn write_map(
    mut output_file: File,
    map: &HashMap<Position, Solid>,
    min_x: usize,
    min_y: usize,
    max_x: usize,
    max_y: usize,
) {
    let width = max_x - min_x + 10;
    let height = max_y - min_y + 10;
    output_file
        .write_all(format!("P3\n{} {}\n255\n", width, height).as_bytes())
        .unwrap();

    for y in min_y..=(min_y + height) {
        let line: Vec<_> = (min_x..(min_x + width))
            .map(|x| match map.get(&(x - 5, y)) {
                None => "51 51 51",
                Some(solid) => match solid {
                    Solid::Rock => "112 128 144", // slate-ish
                    Solid::Sand => "242 211 191",
                },
            })
            .collect();
        output_file
            .write_all((line.join(" ") + "\n").as_bytes())
            .unwrap();
    }
}

/// solves the problem for day 14
pub fn day14(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> usize {
        let (mut map, ((min_x, mut min_y), (max_x, max_y))) = parse_input_paths(input);
        min_y = min_y.min(0);
        let mut frame_index = 0;
        let mut pouring_sand_path: Vec<Position> = vec![(500, 0)];
        let mut sand_units = 0;
        loop {
            let output_file =
                File::create(format!("./output/frame{}.ppm", frame_index).as_str()).unwrap();
            write_map(output_file, &map, min_x, min_y, max_x, max_y);
            frame_index += 1;
            let (x, y) = pouring_sand_path.last().unwrap();
            // past the last path?
            if *y > max_y {
                break;
            }
            if map.get(&(*x, *y + 1)).is_none() {
                // fall straight down
                pouring_sand_path.push((*x, *y + 1));
            } else if map.get(&(*x - 1, *y + 1)).is_none() {
                // fall down and to the left
                pouring_sand_path.push((*x - 1, *y + 1));
            } else if map.get(&(*x + 1, *y + 1)).is_none() {
                // fall down and to the right
                pouring_sand_path.push((*x + 1, *y + 1));
            } else {
                // come to rest here / next unit of sand
                map.insert((*x, *y), Solid::Sand);
                pouring_sand_path.pop();
                sand_units += 1;
            }
        }
        sand_units
    }

    fn part2(input: &str) -> usize {
        let (mut map, ((min_x, mut min_y), (max_x, max_y))) = parse_input_paths(input);
        min_y = min_y.min(0);
        let mut frame_index = 0;
        let mut pouring_sand_path: Vec<Position> = vec![(500, 0)];
        let mut sand_units = 0;
        loop {
            let output_file =
                File::create(format!("./output/frame{}.ppm", frame_index).as_str()).unwrap();
            write_map(output_file, &map, min_x, min_y, max_x, max_y);
            frame_index += 1;
            let last = pouring_sand_path.last();
            // filled input?
            if last.is_none() {
                break;
            }
            let (x, y) = last.unwrap();
            // reached floor?
            if *y > max_y {
                map.insert((*x, *y), Solid::Sand);
                map.insert((*x, *y + 1), Solid::Rock);
                pouring_sand_path.pop();
                sand_units += 1;
            } else if map.get(&(*x, *y + 1)).is_none() {
                // fall straight down
                pouring_sand_path.push((*x, *y + 1));
            } else if map.get(&(*x - 1, *y + 1)).is_none() {
                // fall down and to the left
                pouring_sand_path.push((*x - 1, *y + 1));
            } else if map.get(&(*x + 1, *y + 1)).is_none() {
                // fall down and to the right
                pouring_sand_path.push((*x + 1, *y + 1));
            } else {
                // come to rest here / next unit of sand
                map.insert((*x, *y), Solid::Sand);
                pouring_sand_path.pop();
                sand_units += 1;
            }
        }
        sand_units
    }

    Solution::USize(match part {
        Part::One => part1(input),
        Part::Two => part2(input),
    })
}
