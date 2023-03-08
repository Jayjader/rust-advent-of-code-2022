use std::collections::HashSet;
use std::str::FromStr;

use crate::{Part, Solution};

enum Direction {
    Left,
    Right,
    Up,
    Down,
}
impl FromStr for Direction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "L" => Ok(Direction::Left),
            "R" => Ok(Direction::Right),
            "U" => Ok(Direction::Up),
            "D" => Ok(Direction::Down),
            _ => Err(()),
        }
    }
}
/// common parsing for both parts
fn parse_input(input: &'_ str) -> impl Iterator<Item = (Direction, usize)> + '_ {
    input
        .split_terminator('\n')
        .map(|motion| {
            motion
                .split_once(' ')
                .expect("no space in line to split on")
        })
        .map(|(dir, dist)| {
            (
                dir.parse::<Direction>().unwrap(),
                dist.parse::<usize>().unwrap(),
            )
        })
}

/// solves the problem for day 9
pub fn day9(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> usize {
        let (mut head, mut tail) = ((0, 0), (0, 0));
        let mut positions = HashSet::new();
        positions.insert(tail);
        for (direction, distance) in parse_input(input) {
            let head_offset = match direction {
                Direction::Left => (-1, 0),
                Direction::Right => (1, 0),
                Direction::Up => (0, 1),
                Direction::Down => (0, -1),
            };
            for _ in 0..distance {
                // move head
                head = (head.0 + head_offset.0, head.1 + head_offset.1);
                // move tail to catch up if needed
                let tail_offset = (head.0 - tail.0, head.1 - tail.1);
                match tail_offset {
                    (x, y) if isize::abs(x) <= 1 && isize::abs(y) <= 1 => {
                        // tail is still adjacent to head after head moved, so tail stays put
                    }
                    (x, y) if isize::abs(x) > 1 || isize::abs(y) > 1 => {
                        tail.0 += x.signum();
                        tail.1 += y.signum();
                    }
                    other => {
                        panic!("uh-oh, tail detached from head: {:?}", other)
                    }
                }
                positions.insert(tail);
            }
        }
        positions.len()
    }
    fn part2(input: &str) -> usize {
        let mut rope: [(isize, isize); 10] = [
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
        ];
        let mut positions = HashSet::new();
        positions.insert(rope[9]);
        for (direction, distance) in parse_input(input) {
            let head_offset = match direction {
                Direction::Left => (-1, 0),
                Direction::Right => (1, 0),
                Direction::Up => (0, 1),
                Direction::Down => (0, -1),
            };
            for _ in 0..distance {
                // move head
                rope[0] = (rope[0].0 + head_offset.0, rope[0].1 + head_offset.1);
                // propagate movement down the rope segments
                for i in 1..10 {
                    let segment_offset = (rope[i - 1].0 - rope[i].0, rope[i - 1].1 - rope[i].1);
                    // move segment to catch up if needed
                    match segment_offset {
                        (x, y) if isize::abs(x) > 1 || isize::abs(y) > 1 => {
                            rope[i].0 += x.signum();
                            rope[i].1 += y.signum();
                        }
                        (x, y) if isize::abs(x) <= 1 && isize::abs(y) <= 1 => {
                            // segment is still adjacent to previous after previous moved, so segment stays put
                            break;
                        }
                        other => {
                            panic!("uh-oh, segment {} detached from previous: {:?}", i, other)
                        }
                    }
                }
                positions.insert(rope[9]);
            }
        }
        positions.len()
    }

    match part {
        Part::One => Solution::USize(part1(input)),
        Part::Two => Solution::USize(part2(input)),
    }
}
