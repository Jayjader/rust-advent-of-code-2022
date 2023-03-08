use std::cmp::Ordering;
use std::collections::{HashSet, VecDeque};
use std::ops::Rem;

use regex::Regex;

use crate::{Part, Solution};

type Position = (isize, isize);

fn parse_input_map(input: &str) -> (HashSet<Position>, Vec<(Position, Position)>) {
    let re = Regex::new(r"^Sensor at x=(?P<sensor_x>-*\d+), y=(?P<sensor_y>-*\d+): closest beacon is at x=(?P<beacon_x>-*\d+), y=(?P<beacon_y>-*\d+)").unwrap();
    input
        .lines()
        .flat_map(|line| re.captures(line))
        .map(|captures| {
            (
                (
                    captures["sensor_x"].parse::<isize>().unwrap(),
                    captures["sensor_y"].parse::<isize>().unwrap(),
                ),
                (
                    captures["beacon_x"].parse::<isize>().unwrap(),
                    captures["beacon_y"].parse::<isize>().unwrap(),
                ),
            )
        })
        .fold(
            (HashSet::new(), Vec::new()),
            |(mut map, mut sensors), ((s_x, s_y), (b_x, b_y))| {
                map.insert((b_x, b_y));
                sensors.push(((s_x, s_y), (b_x, b_y)));
                (map, sensors)
            },
        )
}

/// solves the problem for day 15
pub fn day15(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> usize {
        const QUERIED_Y: isize = 2_000_000;
        // const QUERIED_Y: isize = 10;
        let (beacons, sensors) = parse_input_map(input);
        let mut queried_line = HashSet::<isize>::new();
        for ((sensor_x, sensor_y), (beacon_x, beacon_y)) in sensors.iter().cloned() {
            let sensor_clear_radius =
                (beacon_x.abs_diff(sensor_x) + beacon_y.abs_diff(sensor_y)) as isize;
            let y_dist = QUERIED_Y.abs_diff(sensor_y) as isize;
            match y_dist.cmp(&sensor_clear_radius) {
                Ordering::Equal => {
                    if !beacons.contains(&(sensor_x, QUERIED_Y)) {
                        queried_line.insert(sensor_x);
                    }
                }
                Ordering::Less => {
                    let dx = sensor_clear_radius - y_dist;
                    for x in (sensor_x - dx)..=(sensor_x + dx) {
                        if !beacons.contains(&(x, QUERIED_Y)) {
                            queried_line.insert(x);
                        }
                    }
                }
                Ordering::Greater => {}
            }
        }
        queried_line.len()
    }
    fn part2(input: &str) -> u64 {
        fn insert_point(
            mut vec: VecDeque<std::ops::RangeInclusive<isize>>,
            point: isize,
        ) -> VecDeque<std::ops::RangeInclusive<isize>> {
            if vec.is_empty() {
                vec.push_front(point..=point);
                return vec;
            }
            let existing_range = vec.front().unwrap();
            if point + 1 < *existing_range.start() {
                vec.push_front(point..=point);
                return vec;
            }
            if point + 1 == *existing_range.start() {
                let front = vec.pop_front().unwrap();
                vec.push_front(point..=*front.end());
                return vec;
            }
            if point >= *existing_range.start() && point <= *existing_range.end() {
                // noop, point is already inside range
                return vec;
            }
            if point - 1 == *existing_range.end() {
                let front = vec.pop_front().unwrap();
                if let Some(following) = vec.front() {
                    if *following.start() == point + 1 {
                        // point is the exact value needed to merge the 2 adjacent ranges
                        let following = vec.pop_front().unwrap();
                        vec.push_front(*front.start()..=*following.end());
                        return vec;
                    }
                    vec.push_front(*front.start()..=point);
                    return vec;
                }
                vec.push_front(*front.start()..=point);
                return vec;
            }
            // effectively, return [head, rec_call(tail, point)]
            let front = vec.pop_front().unwrap();
            let mut recursive_call = insert_point(vec, point);
            recursive_call.push_front(front);
            recursive_call
        }

        fn insert_range(
            mut deque: VecDeque<std::ops::RangeInclusive<isize>>,
            new_range: std::ops::RangeInclusive<isize>,
        ) -> VecDeque<std::ops::RangeInclusive<isize>> {
            if deque.is_empty() {
                deque.push_front(new_range);
                return deque;
            }
            let current_range = deque.front().unwrap();
            if *new_range.end() < (*current_range.start() - 1) {
                deque.push_front(new_range);
                return deque;
            }
            if *new_range.end() == (*current_range.start() - 1)
                || (*new_range.start() <= *current_range.start())
                    && (*new_range.end() >= *current_range.start())
                    && (*new_range.end() <= *current_range.end())
            {
                let front = deque.pop_front().unwrap();
                let new_front = *new_range.start()..=*front.end();
                return insert_range(deque, new_front);
            }
            if (*new_range.start() <= *current_range.start())
                && (*new_range.end() >= *current_range.end())
            {
                deque.pop_front();
                return insert_range(deque, new_range);
            }
            if (*new_range.start() >= *current_range.start())
                && (*new_range.start() <= *current_range.end())
                && (*new_range.end() >= *current_range.end())
                || *new_range.start() == (*current_range.end() + 1)
            {
                let front = deque.pop_front().unwrap();
                let new_front = *front.start()..=*new_range.end();
                return insert_range(deque, new_front);
            }
            if (*new_range.start() >= *current_range.start())
                && (*new_range.end() <= *current_range.end())
            {
                // noop, new_range is completely contained by range already present in deque
                return deque;
            }
            // effectively, return [head, rec_call(tail, range)]
            let front = deque.pop_front().unwrap();
            let mut recursive_call = insert_range(deque, new_range);
            recursive_call.push_front(front);
            recursive_call
        }

        const MAX_X: isize = 4_000_000;
        // const MAX_X: isize = 20;
        const MIN_X: isize = 0;
        const MAX_Y: isize = 4_000_000;
        // const MAX_Y: isize = 20;
        const MIN_Y: isize = 0;
        let (beacons, sensors) = parse_input_map(input);
        let (mut x, mut y): (u64, u64) = (0, 0);
        'y_querying: for queried_y in MIN_Y..=MAX_Y {
            if queried_y.rem(250_000) == 0 {
                println!("querying y == {}", queried_y);
            }
            let mut queried_line = VecDeque::<std::ops::RangeInclusive<isize>>::new();
            for ((sensor_x, sensor_y), (beacon_x, beacon_y)) in sensors.iter().cloned() {
                let sensor_clear_radius =
                    (beacon_x.abs_diff(sensor_x) + beacon_y.abs_diff(sensor_y)) as isize;
                let y_dist = queried_y.abs_diff(sensor_y) as isize;
                match y_dist.cmp(&sensor_clear_radius) {
                    Ordering::Equal => {
                        if (MIN_X..=MAX_X).contains(&sensor_x)
                            && !beacons.contains(&(sensor_x, queried_y))
                        {
                            queried_line = insert_point(queried_line, sensor_x);
                        }
                    }
                    Ordering::Less => {
                        let dx = sensor_clear_radius - y_dist;
                        let new_start = sensor_x - dx;
                        let new_end = sensor_x + dx;
                        if (MIN_X..=MAX_X).contains(&new_start)
                            || (MIN_X..=MAX_X).contains(&new_end)
                        {
                            let new_start = new_start.clamp(MIN_X, MAX_X);
                            let new_end = new_end.clamp(MIN_X, MAX_X);
                            queried_line = insert_range(queried_line, new_start..=new_end);
                        }
                    }
                    Ordering::Greater => {}
                }
            }
            let number_of_impossible_positions = queried_line
                .iter()
                .map(|range| (*range.end() - *range.start() + 1) as usize)
                .sum::<usize>();
            if number_of_impossible_positions == ((MAX_X - MIN_X + 1) - 1) as usize {
                y = queried_y as u64;
                x = (MIN_X..=MAX_X)
                    .find(|x_| queried_line.iter().all(|range| !range.contains(x_)))
                    .unwrap() as u64;
                println!("found beacon position: ({}, {})", x, y);
                break 'y_querying;
            }
        }
        4_000_000u64 * x + y
    }
    match part {
        Part::One => Solution::USize(part1(input)),
        Part::Two => Solution::U64(part2(input)),
    }
}
