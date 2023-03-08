use std::cmp::{Ordering, Reverse};
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fs::File;
use std::io::Write;

use crate::{Part, Solution};

type Position = (isize, isize);
fn parse_height_map(input: &str) -> (HashMap<Position, (usize, char)>, (usize, usize)) {
    let width = input.find('\n').unwrap();
    let height = input.len() / width;
    let map = input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars().enumerate().map(move |(x, height_char)| {
                (
                    (x as isize, y as isize),
                    height_char,
                    match height_char {
                        'S' => 1,
                        'E' => 26,
                        _ => height_char as usize - 96,
                    },
                )
            })
        })
        .fold(HashMap::new(), |mut map, (coords, char, height)| {
            map.insert(coords, (height, char));
            map
        });
    (map, (width, height))
}

/// ```
/// assert!(Distance::Int(0) < Distance::Infinity);
/// assert!(Distance::Int(10) < Distance::Int(30));
/// assert!(Distance::Infinity > Distance::Int(0));
/// ```
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum Distance {
    Int(usize),
    Infinity,
}

/// ```
/// assert!(
///     Unvisited {
///         position: (0, 0),
///         distance: Distance::Int(0)
///     } < Unvisited {
///         position: (6, 0),
///         distance: Distance::Infinity
///     }
/// );
/// ```
#[derive(Debug, Eq, Clone, Copy)]
struct Unvisited {
    position: (isize, isize),
    distance: Distance,
}
impl PartialEq for Unvisited {
    fn eq(&self, other: &Self) -> bool {
        self.distance == other.distance
    }
}
impl PartialOrd for Unvisited {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.distance.partial_cmp(&other.distance)
    }

    fn lt(&self, other: &Self) -> bool {
        self.distance.lt(&other.distance)
    }

    fn le(&self, other: &Self) -> bool {
        self.distance.le(&other.distance)
    }

    fn gt(&self, other: &Self) -> bool {
        self.distance.gt(&other.distance)
    }

    fn ge(&self, other: &Self) -> bool {
        self.distance.ge(&other.distance)
    }
}
impl Ord for Unvisited {
    fn cmp(&self, other: &Self) -> Ordering {
        self.distance.cmp(&other.distance)
    }

    fn max(self, other: Self) -> Self {
        if self.distance.max(other.distance) == self.distance {
            self
        } else {
            other
        }
    }

    fn min(self, other: Self) -> Self {
        if self.distance.min(other.distance) == self.distance {
            self
        } else {
            other
        }
    }

    fn clamp(self, min: Self, max: Self) -> Self {
        Self {
            position: self.position,
            distance: self.distance.clamp(min.distance, max.distance),
        }
    }
}

/// Renders a distance map as a PPM image file
fn write_distance_map(
    mut output_file: File,
    map: &HashMap<Position, (Distance, Option<Position>)>,
    dimensions: (usize, usize),
) {
    let (width, height) = dimensions;
    let max_dist = map
        .iter()
        .flat_map(|(_coords, (dist, _))| match dist {
            Distance::Int(dist) => Some(dist),
            Distance::Infinity => None,
        })
        .max()
        .unwrap()
        .max(&255);
    println!("max dist: {}", max_dist);
    output_file
        .write_all(format!("P3\n{} {}\n{}\n", width, height, 255).as_bytes())
        .unwrap();
    for y in 0..height {
        let line: Vec<_> =
            ((0..width).map(|x| match map.get(&(x as isize, y as isize)).unwrap().0 {
                Distance::Int(dist) => {
                    let normalized_dist = dist * 255 / max_dist;
                    format!(
                        "{} {} {}",
                        normalized_dist, normalized_dist, normalized_dist
                    )
                }
                Distance::Infinity => String::from("255 127 0"),
            }))
            .collect();
        let line = line.join(" ") + "\n";
        output_file.write_all(line.as_bytes()).unwrap();
    }
}

/// Calculates the distance to a specific point from every point in a height map.
/// Creates an image file that visualizes the currently calculated distances, for each "iteration" of the internal algorithm.
/// Used for solution to both parts.
fn distances_to_point(
    height_map: &HashMap<Position, (usize, char)>,
    dimensions: (usize, usize),
    point: Position,
) -> HashMap<Position, (Distance, Option<Position>)> {
    let mut min_distance_precedence_map = HashMap::with_capacity(height_map.len());
    let mut to_visit = BinaryHeap::with_capacity(height_map.len());
    for (coords, (_height, _char)) in height_map.iter() {
        min_distance_precedence_map.insert(
            *coords,
            if *coords == point {
                to_visit.push(Reverse(Unvisited {
                    position: *coords,
                    distance: Distance::Int(0),
                }));
                (Distance::Int(0), None)
            } else {
                // unvisited.push(Reverse((coords, Distance::Infinity)));
                (Distance::Infinity, None)
            },
        );
    }
    let mut frame_index = 0;
    while !to_visit.is_empty() {
        println!("=== frame {} ===", frame_index);
        write_distance_map(
            File::create(format!("./output/frame{}.ppm", frame_index).as_str()).unwrap(),
            &min_distance_precedence_map,
            dimensions,
        );
        let next_visited = to_visit.pop().unwrap();
        let (x, y) = next_visited.0.position;
        let (current_best_dist, _prev) = min_distance_precedence_map.get(&(x, y)).unwrap();
        let distance_coming_from_current_visited = Distance::Int(match current_best_dist {
            Distance::Infinity => unreachable!(),
            Distance::Int(d) => d + 1,
        });
        let unvisited = HashSet::from_iter(min_distance_precedence_map.iter().filter_map(
            |(coords, distance)| {
                if distance.0 == Distance::Infinity {
                    Some(*coords)
                } else {
                    None
                }
            },
        ));
        let neighbors =
            HashSet::<(isize, isize)>::from([(x + 1, y), (x, y + 1), (x, y - 1), (x - 1, y)]);
        for coords in neighbors.intersection(&unvisited).cloned() {
            let old_dist = min_distance_precedence_map.get(&coords).unwrap().0;
            let new_dist = if height_map.get(&coords).unwrap().0 as isize
                - height_map.get(&(x, y)).unwrap().0 as isize
                <= 1isize
            {
                distance_coming_from_current_visited.min(old_dist)
            } else {
                Distance::Infinity
            };
            if new_dist < old_dist {
                min_distance_precedence_map.insert(coords, (new_dist, Some((x, y))));
                to_visit.push(Reverse(Unvisited {
                    position: coords,
                    distance: new_dist,
                }));
            }
        }
        frame_index += 1;
    }
    min_distance_precedence_map
}

/// solves the problem for day 12
pub fn day12(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> usize {
        let (heights, dimensions) = parse_height_map(input);
        let start = heights
            .iter()
            .find(|(_coords, (_height, char))| *char == 'S')
            .unwrap();
        let end = heights
            .iter()
            .find(|(_coords, (_height, char))| *char == 'E')
            .unwrap();
        println!("start: {:?}; end: {:?}", &start, &end);
        println!("dimensions: {:?}", dimensions);
        let distance_to_start = distances_to_point(&heights, dimensions, *start.0);

        let mut path_to_end = Vec::new();
        let mut head_of_path = *end.0;
        loop {
            path_to_end.push(head_of_path);
            if let Some(prev) = distance_to_start.get(&head_of_path).unwrap().1 {
                head_of_path = prev;
            } else {
                break;
            }
        }

        match distance_to_start.get(end.0).unwrap().0 {
            Distance::Infinity => unreachable!(),
            Distance::Int(dist) => dist,
        }
    }
    fn part2(input: &str) -> usize {
        let (heights, dimensions) = parse_height_map(input);
        let starting_positions = heights
            .iter()
            .filter(|(_coords, (height, _prev))| *height == 1)
            .map(|(coords, (_height, _prev))| coords)
            .collect::<Vec<_>>();
        println!("{} starting positions found.", starting_positions.len());
        let end = heights
            .iter()
            .find(|(_coords, (_height, char))| *char == 'E')
            .unwrap()
            .0;
        starting_positions
            .iter()
            .flat_map(|coords| {
                let distances = distances_to_point(&heights, dimensions, **coords);
                match distances.get(end).unwrap().0 {
                    Distance::Int(dist) => Some(dist),
                    Distance::Infinity => None,
                }
            })
            .min()
            .unwrap()
    }
    match part {
        Part::One => Solution::USize(part1(input)),
        Part::Two => Solution::USize(part2(input)),
    }
}
