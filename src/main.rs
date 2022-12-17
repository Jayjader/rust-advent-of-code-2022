extern crate core;

use std::cmp::{Ordering, Reverse};
use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Map;
use std::str::{FromStr, Split};
use std::{env, fs};

use regex::Regex;

enum Part {
    One,
    Two,
}

#[derive(Debug, Clone)]
struct PartParseError(usize);

impl TryInto<Part> for usize {
    type Error = PartParseError;

    fn try_into(self) -> Result<Part, Self::Error> {
        match self {
            1 => Ok(Part::One),
            2 => Ok(Part::Two),
            _ => Err(PartParseError(self)),
        }
    }
}

impl Display for PartParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "couldn't parse problem part '{}'", self.0)
    }
}

impl Error for PartParseError {}

/// Solution is either a usize, isize, or string
#[derive(Debug)]
enum Solution {
    UNumber(usize),
    INumber(isize),
    String(String),
}
impl Display for Solution {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Solution::UNumber(n) => {
                write!(f, "{}", n)
            }
            Solution::INumber(n) => {
                write!(f, "{}", n)
            }
            Solution::String(s) => {
                write!(f, "{}", s)
            }
        }
    }
}

/// solve problem for day 1
fn day1(input: &str, part: Part) -> Solution {
    /// Takes a list of inventories, separated by blank lines, of calories, separated by newlines, as input.
    /// Returns an iterator over the sum of each inventory.
    /// (common sub-problem for both parts of day 1)
    fn parse_inventory_totals(input: &str) -> Map<Split<&str>, fn(&str) -> usize> {
        input.split("\n\n").map(|inventory_as_string| {
            inventory_as_string
                .split('\n')
                // parse is sometimes being fed something empty here, I don't know why
                // but just ignoring it gives the correct result anyways
                // so it's probably the nested splitting that is leaving behind an empty string or something
                .flat_map(|calories| calories.parse::<usize>())
                .sum()
        })
    }

    /// Takes a list of inventories, separated by blank lines, of calories, separated by newlines, as input.
    /// Returns the sum of the one containing the most calories.
    fn part1(input: &str) -> usize {
        parse_inventory_totals(input)
            .max()
            .expect("couldn't calculate max!")
    }

    /// Takes a list of inventories, separated by blank lines, of calories, separated by newlines, as input.
    /// Returns the sum of the top 3 largest inventory sums.
    fn part2(input: &str) -> usize {
        let mut max = [0, 0, 0];
        for calories in parse_inventory_totals(input) {
            // sorting guarantees that if calories is bigger than max[0] then it is among the top 3.
            // additionally, sorting guarantees that max[0] is the smallest and thus should always be dropped when a new max is found.
            if calories > max[0] {
                max[0] = calories;
                max.sort();
            }
        }
        max.iter().sum()
    }

    match part {
        Part::One => Solution::UNumber(part1(input)),
        Part::Two => Solution::UNumber(part2(input)),
    }
}

/// solve problem for day 2
fn day2(input: &str, part: Part) -> Solution {
    enum Shape {
        Rock,
        Paper,
        Scissors,
    }
    enum Outcome {
        Lose,
        Draw,
        Win,
    }

    impl TryInto<Shape> for char {
        type Error = char;

        fn try_into(self) -> Result<Shape, Self::Error> {
            match self {
                'A' => Ok(Shape::Rock),
                'B' => Ok(Shape::Paper),
                'C' => Ok(Shape::Scissors),
                'X' => Ok(Shape::Rock),
                'Y' => Ok(Shape::Paper),
                'Z' => Ok(Shape::Scissors),
                c => Err(c),
            }
        }
    }
    impl TryInto<Outcome> for char {
        type Error = char;

        fn try_into(self) -> Result<Outcome, Self::Error> {
            match self {
                'X' => Ok(Outcome::Lose),
                'Y' => Ok(Outcome::Draw),
                'Z' => Ok(Outcome::Win),
                c => Err(c),
            }
        }
    }
    trait Scored {
        fn score(thing: &Self) -> usize;
    }

    impl Scored for Shape {
        fn score(s: &Shape) -> usize {
            match s {
                Shape::Rock => 1,
                Shape::Paper => 2,
                Shape::Scissors => 3,
            }
        }
    }
    impl Scored for Outcome {
        fn score(o: &Outcome) -> usize {
            match o {
                Outcome::Lose => 0,
                Outcome::Draw => 3,
                Outcome::Win => 6,
            }
        }
    }
    fn part1(input: &str) -> usize {
        input.lines().fold(0, |accu, line| {
            let mut chars = line.chars();
            let opponent_move: Shape = chars
                .next()
                .expect("line missing first character")
                .try_into()
                .expect("character is not shape");
            chars.next().expect("line missing second character");
            let my_move: Shape = chars
                .next()
                .expect("line missing third character")
                .try_into()
                .expect("character is not outcome");
            let outcome = match (&my_move, &opponent_move) {
                (Shape::Rock, Shape::Rock) => Outcome::Draw,
                (Shape::Paper, Shape::Paper) => Outcome::Draw,
                (Shape::Scissors, Shape::Scissors) => Outcome::Draw,
                (Shape::Rock, Shape::Paper) => Outcome::Lose,
                (Shape::Paper, Shape::Rock) => Outcome::Win,
                (Shape::Scissors, Shape::Paper) => Outcome::Win,
                (Shape::Paper, Shape::Scissors) => Outcome::Lose,
                (Shape::Rock, Shape::Scissors) => Outcome::Win,
                (Shape::Scissors, Shape::Rock) => Outcome::Lose,
            };
            accu + Scored::score(&outcome) + Scored::score(&my_move)
        })
    }
    fn part2(input: &str) -> usize {
        input.lines().fold(0, |accu, line| {
            let mut chars = line.chars();
            let opponent_move: Shape = chars
                .next()
                .expect("line missing first character")
                .try_into()
                .expect("character is not shape");
            chars.next().expect("line missing second character");
            let desired_outcome: Outcome = chars
                .next()
                .expect("line missing third character")
                .try_into()
                .expect("character is not outcome");
            let my_move = match (&opponent_move, &desired_outcome) {
                (Shape::Rock, Outcome::Lose) => Shape::Scissors,
                (Shape::Rock, Outcome::Draw) => Shape::Rock,
                (Shape::Rock, Outcome::Win) => Shape::Paper,
                (Shape::Paper, Outcome::Lose) => Shape::Rock,
                (Shape::Paper, Outcome::Draw) => Shape::Paper,
                (Shape::Paper, Outcome::Win) => Shape::Scissors,
                (Shape::Scissors, Outcome::Lose) => Shape::Paper,
                (Shape::Scissors, Outcome::Draw) => Shape::Scissors,
                (Shape::Scissors, Outcome::Win) => Shape::Rock,
            };
            accu + Scored::score(&desired_outcome) + Scored::score(&my_move)
        })
    }
    match part {
        Part::One => Solution::UNumber(part1(input)),
        Part::Two => Solution::UNumber(part2(input)),
    }
}

/// solve problem for day 3
fn day3(input: &str, part: Part) -> Solution {
    /// common sub-problem for both parts of day 3
    fn priority_for_char(shared_char: &char) -> usize {
        (*shared_char as u32 as usize)
            - (if shared_char.is_uppercase() {
                65 - 27 // 65 is 'A' which has priority 26
            } else {
                97 - 1 // 97 is 'a' which has priority 1
            })
    }
    fn part1(input: &str) -> usize {
        input
            .lines()
            .map(|rucksack| {
                let (compartment_1, compartment_2) = rucksack.split_at(rucksack.len() / 2);
                let (compartment_1, compartment_2) = (
                    compartment_1.chars().collect::<HashSet<_>>(),
                    compartment_2.chars().collect::<HashSet<_>>(),
                );
                let shared: Vec<&char> = compartment_1.intersection(&compartment_2).collect();
                // safety check that there really is 1 and only 1 shared char
                assert_eq!(shared.len(), 1);

                priority_for_char(shared[0])
            })
            .sum()
    }

    fn part2(input: &str) -> usize {
        // collect into intermediate Vec so that we can call .chunks_exact on it
        // in nightly we could use array_chunk or next_chunk to avoid this intermediate allocation
        let vec_lines: Vec<_> = input.lines().collect();

        vec_lines
            .chunks_exact(3)
            .map(|group_of_rucksacks| {
                group_of_rucksacks
                    .iter()
                    .map(|rucksack| rucksack.chars().collect::<HashSet<char>>())
            })
            .map(|hashed_sacks| {
                let mut common = hashed_sacks.clone().next().unwrap();
                for sack in hashed_sacks {
                    common = common.intersection(&sack).copied().collect();
                }
                // safety check that we found a single common element for each group of 3 rucksacks
                assert_eq!(common.len(), 1);
                common
            })
            .map(|c| priority_for_char(c.iter().collect::<Vec<_>>()[0]))
            .sum()
    }
    match part {
        Part::One => Solution::UNumber(part1(input)),
        Part::Two => Solution::UNumber(part2(input)),
    }
}

/// solves problem for day 4
fn day4(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> usize {
        input
            .lines()
            .flat_map(|line| line.split_once(','))
            .map(|(section1, section2)| {
                (
                    section1.split_once('-').unwrap(),
                    section2.split_once('-').unwrap(),
                )
            })
            .map(|((a0, a1), (b0, b1))| {
                (
                    (a0.parse::<usize>().unwrap(), a1.parse::<usize>().unwrap()),
                    (b0.parse::<usize>().unwrap(), b1.parse::<usize>().unwrap()),
                )
            })
            .map(|((start1, end1), (start2, end2))| {
                // bool condition to unsigned int: true is 1 and false is 0
                usize::from(start1 <= start2 && end1 >= end2 || start2 <= start1 && end2 >= end1)
            })
            .sum()
    }

    fn part2(input: &str) -> usize {
        input
            .lines()
            .flat_map(|line| line.split_once(','))
            .map(|(section1, section2)| {
                (
                    section1.split_once('-').unwrap(),
                    section2.split_once('-').unwrap(),
                )
            })
            .map(|((a0, a1), (b0, b1))| {
                (
                    (a0.parse::<usize>().unwrap(), a1.parse::<usize>().unwrap()),
                    (b0.parse::<usize>().unwrap(), b1.parse::<usize>().unwrap()),
                )
            })
            .map(|((start1, end1), (start2, end2))| {
                // bool condition to unsigned int: true is 1 and false is 0
                usize::from(
                    start1 <= start2 && end1 >= start2 || start2 <= start1 && end2 >= start1,
                )
            })
            .sum()
    }

    match part {
        Part::One => Solution::UNumber(part1(input)),
        Part::Two => Solution::UNumber(part2(input)),
    }
}

/// solves problem for day 5
fn day5(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> String {
        let (drawing, commands) = input.split_once("\n\n").unwrap();
        let newline_index = drawing.find('\n').unwrap();
        // a drawing with n stacks has 4n-1 characters per line:
        // 3 per stack (3n) + in between the stacks (n-1)
        let number_of_stacks = (newline_index + 1) / 4;
        let mut stacks = Vec::with_capacity(number_of_stacks);
        stacks.resize_with(number_of_stacks, VecDeque::<char>::new);
        // skip the last line as we can just infer the stack indices
        let end_of_stacks = drawing.rfind('\n').unwrap();
        for line in drawing[0..end_of_stacks].lines() {
            for (index, c) in line.chars().enumerate() {
                // [S] [D]     [W]     [W]     [H] [Q]
                //  |   |       |
                // 0123012301230123...
                //  ^   ^       ^
                if index % 4 == 1 && c.is_alphabetic() {
                    stacks[index / 4].push_back(c)
                }
            }
        }
        // parse command
        let re = Regex::new(r"^move (?P<count>\d+) from (?P<start>\d+) to (?P<end>\d+)$").unwrap();
        for line in commands.lines() {
            if let Some(caps) = re.captures(line) {
                let count = caps["count"].parse::<usize>().unwrap();
                let start = caps["start"].parse::<usize>().unwrap();
                let end = caps["end"].parse::<usize>().unwrap();
                for _ in 0..count {
                    let moved = stacks[start - 1].pop_front().unwrap();
                    stacks[end - 1].push_front(moved);
                }
            }
        }
        stacks.iter().map(|stack| stack[0]).collect::<String>()
    }
    fn part2(input: &str) -> String {
        let (drawing, commands) = input.split_once("\n\n").unwrap();
        let newline_index = drawing.find('\n').unwrap();
        // a drawing with n stacks has 4n-1 characters per line:
        // 3 per stack (3n) + in between the stacks (n-1)
        let number_of_stacks = (newline_index + 1) / 4;
        let mut stacks = Vec::with_capacity(number_of_stacks);
        stacks.resize_with(number_of_stacks, VecDeque::<char>::new);
        // skip the last line as we can just infer the stack indices
        let end_of_stacks = drawing.rfind('\n').unwrap();
        for line in drawing[0..end_of_stacks].lines() {
            for (index, c) in line.chars().enumerate() {
                // [S] [D]     [W]     [W]     [H] [Q]
                //  |   |       |
                // 0123012301230123...
                //  ^   ^       ^
                if index % 4 == 1 && c.is_alphabetic() {
                    stacks[index / 4].push_back(c)
                }
            }
        }
        // parse command
        let re = Regex::new(r"^move (?P<count>\d+) from (?P<start>\d+) to (?P<end>\d+)$").unwrap();
        for line in commands.lines() {
            if let Some(caps) = re.captures(line) {
                let count = caps["count"].parse::<usize>().unwrap();
                let start = caps["start"].parse::<usize>().unwrap();
                let end = caps["end"].parse::<usize>().unwrap();
                let mut to_move = Vec::new();
                for _ in 0..count {
                    to_move.push(stacks[start - 1].pop_front().unwrap());
                }
                for _ in 0..count {
                    stacks[end - 1].push_front(to_move.pop().unwrap());
                }
            }
        }
        stacks.iter().map(|stack| stack[0]).collect::<String>()
    }

    match part {
        Part::One => Solution::String(part1(input)),
        Part::Two => Solution::String(part2(input)),
    }
}

/// solves problem for day 6
fn day6(input: &str, part: Part) -> Solution {
    /// finds the first window of length N over bytes in input str slice that contains no duplicates
    /// return the position (in the overall input slice) of the last byte in the window found
    /// common sub-problem for both parts
    fn slide_window_until_unique<const N: usize>(input: &str) -> usize {
        input
            .as_bytes()
            .windows(N)
            .position(|window| HashSet::<&u8>::from_iter(window).len() == N)
            .unwrap()
            + N
    }
    fn part1(input: &str) -> usize {
        slide_window_until_unique::<4>(input)
    }

    fn part2(input: &str) -> usize {
        slide_window_until_unique::<14>(input)
    }
    match part {
        Part::One => Solution::UNumber(part1(input)),
        Part::Two => Solution::UNumber(part2(input)),
    }
}

/// solves problem for day 7
fn day7(input: &str, part: Part) -> Solution {
    #[derive(Debug, Hash, Eq, PartialEq)]
    enum Command<'a> {
        ChangeDirParent,
        ChangeDirChild(&'a str),
        List,
    }

    impl<'a> TryFrom<&'a str> for Command<'a> {
        type Error = ();
        fn try_from(value: &'a str) -> Result<Self, Self::Error> {
            let (command, arg) = value.split_at(2);
            match command {
                "cd" => match arg.trim() {
                    ".." => Ok(Command::ChangeDirParent),
                    dir => Ok(Command::ChangeDirChild(dir)),
                },
                "ls" => Ok(Command::List),
                _ => Err(()),
            }
        }
    }

    #[derive(Debug)]
    enum DirEntry<'a> {
        Dir(&'a str),
        File(&'a str, usize),
    }

    impl<'a> TryInto<DirEntry<'a>> for &'a str {
        type Error = ();
        fn try_into(self) -> Result<DirEntry<'a>, Self::Error> {
            let (extra, name) = self.split_once(' ').expect("couldn't split on space");
            match extra {
                "dir" => Ok(DirEntry::Dir(name)),
                _ => Ok(DirEntry::File(
                    name,
                    extra.parse::<usize>().expect("couldn't parse file size"),
                )),
            }
        }
    }
    fn parse_input_and_build_fs_tree<'a>(
        input: &'a str,
        dir_map: &'a mut HashMap<String, Vec<DirEntry<'a>>>,
        cwd: &'a mut Vec<&'a str>,
    ) -> (
        &'a mut HashMap<String, Vec<DirEntry<'a>>>,
        &'a mut Vec<&'a str>,
    ) {
        input
            .split("$ ")
            .skip(1 /*first element of split is empty string*/)
            .map(|command_with_output| {
                let (command, result) = command_with_output.split_once('\n').unwrap();
                let command = Command::try_from(command).unwrap();
                (
                    command,
                    if result.is_empty() {
                        None
                    } else {
                        Some(result)
                    },
                )
            })
            .fold((dir_map, cwd), |(accu, cwd), (command, result)| {
                match command {
                    Command::ChangeDirParent => {
                        cwd.pop();
                    }
                    Command::ChangeDirChild(child) => {
                        cwd.push(child);
                    }
                    Command::List => {
                        accu.insert(
                            cwd.join("/"),
                            result
                                .unwrap()
                                .split_terminator('\n')
                                .flat_map(|single_result| single_result.try_into())
                                .collect(),
                        );
                    }
                };

                (accu, cwd)
            })
    }
    /// calculates total size of a dir
    /// stores all dir sizes in cache to avoid duplicate calculations
    /// also allows inspecting/retrieval of all dir sizes calculated
    fn total_size<'a>(
        tree: &HashMap<String, Vec<DirEntry<'a>>>,
        dir: String,
        cache: &mut HashMap<String, usize>,
    ) -> usize {
        if let Some(cached_size) = cache.get(dir.as_str()) {
            return *cached_size;
        }
        let size = tree
            .get(dir.as_str())
            .expect("dir does not exist in tree")
            .iter()
            .map(|entry| match entry {
                DirEntry::File(_, size) => *size,
                DirEntry::Dir(name) => {
                    let path = [dir.as_str(), *name].join("/");
                    total_size(tree, path, cache)
                }
            })
            .sum();
        cache.insert(dir, size);
        size
    }
    fn part1(input: &str) -> usize {
        let dir_map = &mut HashMap::<String, Vec<DirEntry>>::new();
        let cwd = &mut Vec::new();
        let (accu, _) = parse_input_and_build_fs_tree(input, dir_map, cwd);

        let mut dir_sizes = HashMap::new();

        total_size(accu, String::from("/"), &mut dir_sizes);

        dir_sizes
            .iter()
            .filter_map(|(_, v)| if *v <= 100_000 { Some(v) } else { None })
            .sum()
    }

    fn part2(input: &str) -> usize {
        let dir_map = &mut HashMap::<String, Vec<DirEntry>>::new();
        let cwd = &mut Vec::new();
        let (dir_map, _) = parse_input_and_build_fs_tree(input, dir_map, cwd);

        let mut dir_sizes = HashMap::new();

        total_size(dir_map, String::from("/"), &mut dir_sizes);

        let total_space_used: usize = dir_map
            .values()
            .map(|entries| {
                entries
                    .iter()
                    .filter_map(|entry| match entry {
                        DirEntry::File(_, size) => Some(size),
                        DirEntry::Dir(_) => None,
                    })
                    .sum::<usize>()
            })
            .sum();
        let current_free_size = 70_000_000 - total_space_used;
        let mut sizes = dir_sizes.iter().collect::<Vec<_>>();
        sizes.sort_by(|a, b| (a.1.cmp(b.1)));
        sizes
            .iter()
            .find_map(|dir| {
                if current_free_size + dir.1 > 30_000_000 {
                    Some(*dir.1)
                } else {
                    None
                }
            })
            .unwrap()
    }
    match part {
        Part::One => Solution::UNumber(part1(input)),
        Part::Two => Solution::UNumber(part2(input)),
    }
}

/// solves the problem for day 8
fn day8(input: &str, part: Part) -> Solution {
    fn parse_tree_heights(input: &str) -> Vec<Vec<usize>> {
        input
            .split_terminator('\n')
            .map(|line| line.split("").flat_map(|c| c.parse::<usize>()).collect())
            .collect()
    }
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
        Part::One => Solution::UNumber(part1(input)),
        Part::Two => Solution::UNumber(part2(input)),
    }
}

/// solves the problem for day 9
fn day9(input: &str, part: Part) -> Solution {
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
        Part::One => Solution::UNumber(part1(input)),
        Part::Two => Solution::UNumber(part2(input)),
    }
}

/// solves the problem for day 10
fn day10(input: &str, part: Part) -> Solution {
    #[derive(Debug)]
    enum Instruction {
        Noop,
        AddX(i32),
    }
    impl FromStr for Instruction {
        type Err = ();

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s.chars().take(4).collect::<String>().as_str() {
                "noop" => Ok(Instruction::Noop),
                "addx" => Ok(Instruction::AddX(s.split_at(5).1.parse::<i32>().unwrap())),
                _ => Err(()),
            }
        }
    }
    fn part1(input: &str) -> isize {
        let (_, _, reg_per_cycle) = input
            .lines()
            .flat_map(|line| line.parse::<Instruction>())
            .fold(
                (0, 1, Vec::<i32>::with_capacity(240)),
                |(cycle, register, mut output), instruction| match instruction {
                    Instruction::Noop => {
                        output.push(register);
                        (cycle + 1, register, output)
                    }
                    Instruction::AddX(x) => {
                        output.push(register);
                        output.push(register);
                        (cycle + 2, register + x, output)
                    }
                },
            );
        (20 * reg_per_cycle[20 - 1]
            + 60 * reg_per_cycle[60 - 1]
            + 100 * reg_per_cycle[100 - 1]
            + 140 * reg_per_cycle[140 - 1]
            + 180 * reg_per_cycle[180 - 1]
            + 220 * reg_per_cycle[220 - 1]) as isize
    }
    fn part2(input: &str) -> String {
        fn screen_coords_during_cycle(cycle: usize) -> (usize, usize) {
            (cycle % 40, cycle / 40)
        }
        let mut screen = vec![vec!["."; 40]; 6];
        let (_, _, _) = input
            .lines()
            .flat_map(|line| line.parse::<Instruction>())
            .fold(
                (0isize, 1isize, Vec::<isize>::with_capacity(240)),
                |(cycle, register, mut output), instruction| {
                    let (x, y) = screen_coords_during_cycle(cycle as usize);
                    if [register - 1, register, register + 1].contains(&(x as isize)) {
                        screen[y][x] = "#"
                    }
                    match instruction {
                        Instruction::Noop => {
                            output.push(register);
                            (cycle + 1, register, output)
                        }
                        Instruction::AddX(x_arg) => {
                            output.push(register);
                            output.push(register);
                            let (x, y) = screen_coords_during_cycle((cycle + 1) as usize);
                            if [register - 1, register, register + 1].contains(&(x as isize)) {
                                screen[y][x] = "#"
                            }
                            (cycle + 2, register + x_arg as isize, output)
                        }
                    }
                },
            );
        screen
            .iter()
            .map(|line| line.join(""))
            .collect::<Vec<_>>()
            .join("\n")
    }

    match part {
        Part::One => Solution::INumber(part1(input)),
        Part::Two => Solution::String(part2(input)),
    }
}

/// solves the problem for day 11
fn day11(input: &str, part: Part) -> Solution {
    #[derive(Debug, Clone, Copy)]
    struct Test {
        divisor: u64,
        true_: usize,
        false_: usize,
    }
    impl FromStr for Test {
        type Err = ();
        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (divisor, rest) = s
                .split_once("Test: divisible by ")
                .unwrap()
                .1
                .split_once('\n')
                .unwrap();
            let divisor = divisor.parse().unwrap();
            let (true_, false_) = rest.split_once('\n').unwrap();
            let true_ = true_
                .split_once("If true: throw to monkey ")
                .unwrap()
                .1
                .parse()
                .unwrap();
            let false_ = false_
                .split_once("If false: throw to monkey ")
                .unwrap()
                .1
                .parse()
                .unwrap();

            Ok(Test {
                divisor,
                true_,
                false_,
            })
        }
    }
    #[derive(Debug, Clone, Copy)]
    enum Arg {
        Number(u64),
        Old,
    }
    impl FromStr for Arg {
        type Err = ();
        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s {
                "old" => Ok(Arg::Old),
                _ => Ok(Arg::Number(s.parse().unwrap())),
            }
        }
    }
    #[derive(Debug, Clone, Copy)]
    enum Operation {
        Add(Arg),
        Mult(Arg),
    }
    impl FromStr for Operation {
        type Err = ();
        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let data = s.split_once("Operation: new = old ").unwrap().1;
            let (op, arg) = data.split_once(' ').unwrap();
            let arg = arg.parse().unwrap();
            match op {
                "+" => Ok(Operation::Add(arg)),
                "*" => Ok(Operation::Mult(arg)),
                _ => Err(()),
            }
        }
    }
    #[derive(Debug, Clone)]
    struct Monkey {
        inspections: usize,
        items: Vec<u64>,
        operation: Operation,
        test: Test,
    }
    impl FromStr for Monkey {
        type Err = ();

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let (_header, rest) = s.split_once('\n').unwrap();
            let (items, rest) = rest.split_once('\n').unwrap();
            let items = items
                .split_once("Starting items: ")
                .unwrap()
                .1
                .split(", ")
                .flat_map(str::parse)
                .collect();
            let (operation, test) = rest.split_once('\n').unwrap();
            let operation = operation.parse().unwrap();
            let test = test.trim().parse().unwrap();
            Ok(Monkey {
                inspections: 0,
                items,
                operation,
                test,
            })
        }
    }
    fn round_recursively(
        mut has_inspected_and_thrown: VecDeque<Monkey>,
        mut has_not_inspected_nor_thrown: VecDeque<Monkey>,
    ) -> (VecDeque<Monkey>, VecDeque<Monkey>) {
        let next_monkey = has_not_inspected_nor_thrown.pop_front();
        match next_monkey {
            None => (has_inspected_and_thrown, has_not_inspected_nor_thrown),
            Some(monkey) => {
                let mut next_monkey = Monkey { ..monkey };
                next_monkey.inspections += next_monkey.items.len();
                for item_worry in (next_monkey.items).iter() {
                    println!(
                        "  Monkey inspects an item with a worry level of {}.",
                        item_worry
                    );
                    // -> apply operation
                    print!("    Worry level "); // is multiplied by 19 to 1501.");
                    let item_worry = match &monkey.operation {
                        Operation::Add(arg) => {
                            print!("increases by ");
                            match arg {
                                Arg::Number(n) => {
                                    print!("{}", n);
                                    item_worry + n
                                }
                                Arg::Old => {
                                    print!("{}", item_worry);
                                    item_worry + item_worry
                                }
                            }
                        }
                        Operation::Mult(arg) => {
                            print!("is multiplied by ");
                            match arg {
                                Arg::Number(n) => {
                                    print!("{}", n);
                                    item_worry * n
                                }
                                Arg::Old => {
                                    print!("{}", item_worry);
                                    item_worry * item_worry
                                }
                            }
                        }
                    };
                    println!(" to {}", item_worry);
                    // -> reduce by 3, rounded down
                    let item_worry = item_worry / 3;
                    println!(
                        "    Monkey gets bored with item. Worry level is divided by 3 to {}.",
                        item_worry
                    );
                    print!("    Current worry level is ");
                    let receiver_index = if item_worry % monkey.test.divisor == 0 {
                        monkey.test.true_
                    } else {
                        print!("not");
                        monkey.test.false_
                    };
                    println!(" divisible by {}.", monkey.test.divisor);
                    println!(
                        "    Item with worry level {} is thrown to monkey {}",
                        item_worry, receiver_index
                    );
                    if receiver_index >= has_inspected_and_thrown.len() {
                        has_not_inspected_nor_thrown
                            [receiver_index - has_inspected_and_thrown.len() - 1]
                            .items
                            .push(item_worry);
                    } else {
                        has_inspected_and_thrown[receiver_index]
                            .items
                            .push(item_worry);
                    }
                }
                next_monkey.items.clear();

                has_inspected_and_thrown.push_back(next_monkey);
                round_recursively(has_inspected_and_thrown, has_not_inspected_nor_thrown)
            }
        }
    }
    fn part1(input: &str) -> usize {
        let mut monkeys = input
            .split_terminator("\n\n")
            .flat_map(|monkey_input| monkey_input.parse::<Monkey>())
            .collect::<VecDeque<_>>();
        for _ in 1..21 {
            monkeys = round_recursively(VecDeque::with_capacity(monkeys.len()), monkeys).0;
        }
        let mut monkeys = monkeys.into_iter().collect::<Vec<_>>();
        monkeys.sort_by(|m0, m1| m1.inspections.cmp(&m0.inspections));
        monkeys
            .iter()
            .take(2)
            .fold(1, |product, m| m.inspections * product)
    }

    fn round_recursively_ridiculous_levels(
        divisor_product: u64,
        mut has_inspected_and_thrown: VecDeque<Monkey>,
        mut has_not_inspected_nor_thrown: VecDeque<Monkey>,
    ) -> (VecDeque<Monkey>, VecDeque<Monkey>) {
        let next_monkey = has_not_inspected_nor_thrown.pop_front();
        match next_monkey {
            None => (has_inspected_and_thrown, has_not_inspected_nor_thrown),
            Some(monkey) => {
                let mut next_monkey = Monkey { ..monkey };
                next_monkey.inspections += next_monkey.items.len();
                for item_worry in (next_monkey.items).iter() {
                    let next_item_worry = match &monkey.operation {
                        Operation::Add(arg) => {
                            item_worry
                                + match arg {
                                    Arg::Number(n) => n,
                                    Arg::Old => item_worry,
                                }
                        }
                        Operation::Mult(arg) => {
                            (item_worry
                                * (match arg {
                                    Arg::Number(n) => n,
                                    Arg::Old => item_worry,
                                } % divisor_product))
                                % divisor_product
                        }
                    };
                    let receiver_index = if next_item_worry % monkey.test.divisor == 0 {
                        monkey.test.true_
                    } else {
                        monkey.test.false_
                    };
                    if receiver_index >= has_inspected_and_thrown.len() {
                        has_not_inspected_nor_thrown
                            [receiver_index - has_inspected_and_thrown.len() - 1]
                            .items
                            .push(next_item_worry);
                    } else {
                        has_inspected_and_thrown[receiver_index]
                            .items
                            .push(next_item_worry);
                    }
                }
                next_monkey.items.clear();

                has_inspected_and_thrown.push_back(next_monkey);
                round_recursively_ridiculous_levels(
                    divisor_product,
                    has_inspected_and_thrown,
                    has_not_inspected_nor_thrown,
                )
            }
        }
    }
    fn part2(input: &str) -> usize {
        let mut monkeys = input
            .split_terminator("\n\n")
            .flat_map(|monkey_input| monkey_input.parse::<Monkey>())
            .collect::<VecDeque<_>>();
        let divisor_product: u64 = monkeys
            .iter()
            .map(|m| {
                // print!(" {} ", m.test.divisor);
                m.test.divisor
            })
            .product();
        // println!(" * = {} ", divisor_product);
        for i in 1..10_001 {
            monkeys = round_recursively_ridiculous_levels(
                divisor_product,
                VecDeque::with_capacity(monkeys.len()),
                monkeys,
            )
            .0;
            if [
                1, 20, 1_000, 2_000, 3_000, 4_000, 5_000, 6_000, 7_000, 9_000, 10_000,
            ]
            .contains(&i)
            {
                println!("\n== After round {} ==", i);
                for (i, m) in monkeys.iter().enumerate() {
                    println!("Monkey {} inspected items {} times", i, m.inspections);
                }
            }
            // dbg!(&monkeys);
        }
        // dbg!(&monkeys);
        let mut monkeys = monkeys.into_iter().collect::<Vec<_>>();
        monkeys.sort_by(|m0, m1| m1.inspections.cmp(&m0.inspections));
        monkeys
            .iter()
            .take(2)
            .fold(1, |product, m| m.inspections * product)
    }
    match part {
        Part::One => Solution::UNumber(part1(input)),
        Part::Two => Solution::UNumber(part2(input)),
    }
}

/// solves the problem for day 12
fn day12(input: &str, part: Part) -> Solution {
    fn parse_height_map(input: &str) -> HashMap<(isize, isize), (usize, char)> {
        input
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
            })
    }
    fn part1(input: &str) -> usize {
        let heights = parse_height_map(input);
        let start = heights
            .iter()
            .find(|(_coords, (_height, char))| *char == 'S')
            .unwrap();
        let end = heights
            .iter()
            .find(|(_coords, (_height, char))| *char == 'E')
            .unwrap();
        println!("start: {:?}; end: {:?}", &start, &end);
        let mut distance_to_start: HashMap<(isize, isize), (Distance, Option<(isize, isize)>)> =
            HashMap::with_capacity(heights.len());

        /// ```
        /// assert!(Distance::Int(0) < Distance::Infinity);
        /// assert!(Distance::Int(10) < Distance::Int(30));
        /// assert!(Distance::Infinity > Distance::Int(0));
        /// ```
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
        enum Distance {
            Int(isize),
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

        let mut to_visit = BinaryHeap::with_capacity(heights.len());
        for (coords, (_height, char)) in heights.iter() {
            distance_to_start.insert(
                *coords,
                match char {
                    'S' => {
                        to_visit.push(Reverse(Unvisited {
                            position: *coords,
                            distance: Distance::Int(0),
                        }));
                        (Distance::Int(0), None)
                    }
                    _ => {
                        // unvisited.push(Reverse((coords, Distance::Infinity)));
                        (Distance::Infinity, None)
                    }
                },
            );
        }
        while !to_visit.is_empty() {
            let next_visited = to_visit.pop().unwrap();
            let (x, y) = next_visited.0.position;
            let (current_best_dist, _prev) = distance_to_start.get(&(x, y)).unwrap();
            let distance_coming_from_current_visited = Distance::Int(match current_best_dist {
                Distance::Infinity => unreachable!(),
                Distance::Int(d) => d + 1,
            });
            let unvisited =
                HashSet::from_iter(distance_to_start.iter().filter_map(|(coords, distance)| {
                    if distance.0 == Distance::Infinity {
                        Some(*coords)
                    } else {
                        None
                    }
                }));
            let neighbors =
                HashSet::<(isize, isize)>::from([(x + 1, y), (x, y + 1), (x, y - 1), (x - 1, y)]);
            for coords in neighbors.intersection(&unvisited).cloned() {
                let old_dist = distance_to_start.get(&coords).unwrap().0;
                let new_dist = if heights.get(&coords).unwrap().0 as isize
                    - heights.get(&(x, y)).unwrap().0 as isize
                    <= 1isize
                {
                    distance_coming_from_current_visited.min(old_dist)
                } else {
                    Distance::Infinity
                };
                if new_dist < old_dist {
                    // println!(
                    //     "new shortest path to ({},{}) found (dist={:?}, prev=({},{})",
                    //     coords.0, coords.1, new_dist, x, y
                    // );
                    distance_to_start.insert(coords, (new_dist, Some((x, y))));
                    to_visit.push(Reverse(Unvisited {
                        position: coords,
                        distance: new_dist,
                    }));
                }
            }
        }
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

        path_to_end.len() - 1
    }
    fn part2(_input: &str) -> usize {
        0
    }
    match part {
        Part::One => Solution::UNumber(part1(input)),
        Part::Two => Solution::UNumber(part2(input)),
    }
}

/// solves the problem for day13
fn day13(input: &str, part: Part) -> Solution {
    fn tokenize(input: &str) -> Vec<String> {
        input
            .replace('[', " [ ")
            .replace(']', " ] ")
            .replace(',', " , ")
            .split_whitespace()
            .map(str::to_owned)
            .collect()
    }
    #[derive(Debug, Clone)]
    enum PacketData {
        List(Vec<PacketData>),
        Int(usize),
    }

    fn simple_rec<'a>(
        mut result: Vec<PacketData>,
        tokens: &'a [&'a str],
    ) -> (Vec<PacketData>, Option<&'a [&'a str]>) {
        match tokens.split_first() {
            None => (result, None),
            Some((token, rest)) => match token {
                &"," => simple_rec(result, rest),
                &"]" => (result, Some(rest)),
                &"[" => {
                    let new_list = Vec::new();

                    let (updated_new_list, rest) = simple_rec(new_list, rest);
                    result.push(PacketData::List(updated_new_list));
                    simple_rec(result, rest.unwrap())
                }
                int if int.parse::<usize>().is_ok() => {
                    result.push(PacketData::Int(int.parse().unwrap()));
                    simple_rec(result, rest)
                }
                _ => unreachable!(),
            },
        }
    }

    fn ordered(p1: &PacketData, p2: &PacketData) -> bool {
        in_place_ordering(Ordering::Equal, p1, p2) == Ordering::Less
    }
    fn in_place_ordering(result: Ordering, p1: &PacketData, p2: &PacketData) -> Ordering {
        match result {
            Ordering::Greater => Ordering::Greater,
            Ordering::Less => Ordering::Less,
            Ordering::Equal => match (p1, p2) {
                (PacketData::Int(self_int), PacketData::Int(other_int)) => self_int.cmp(other_int),
                (PacketData::Int(self_int), PacketData::List(_)) => in_place_ordering(
                    result,
                    &PacketData::List(vec![PacketData::Int(*self_int)]),
                    p2,
                ),
                (PacketData::List(_), PacketData::Int(other_int)) => in_place_ordering(
                    result,
                    p1,
                    &PacketData::List(vec![PacketData::Int(*other_int)]),
                ),
                (PacketData::List(self_list), PacketData::List(other_list)) => {
                    match (self_list.first(), other_list.first()) {
                        (None, None) => result,
                        (None, Some(_)) => Ordering::Less,
                        (Some(_), None) => Ordering::Greater,
                        (Some(self_first), Some(other_first)) => {
                            match in_place_ordering(result, self_first, other_first) {
                                Ordering::Less => Ordering::Less,
                                Ordering::Greater => Ordering::Greater,
                                Ordering::Equal => in_place_ordering(
                                    Ordering::Equal,
                                    &PacketData::List(self_list.iter().skip(1).cloned().collect()),
                                    &PacketData::List(other_list.iter().skip(1).cloned().collect()),
                                ),
                            }
                        }
                    }
                }
            },
        }
    }

    fn part1(input: &str) -> usize {
        input
            .split_terminator("\n\n")
            .enumerate()
            .flat_map(|(index, pair)| {
                let (first, second) = pair.split_once('\n').expect("need newline between pair");
                let tokenized_first = tokenize(first);
                let first = tokenized_first
                    .iter()
                    .skip(1)
                    .map(String::as_str)
                    .collect::<Vec<_>>();
                let first_vec = Vec::new();
                let (first, _) = simple_rec(first_vec, first.as_slice());
                let tokenized_second = tokenize(second);
                let second = tokenized_second
                    .iter()
                    .skip(1)
                    .map(String::as_str)
                    .collect::<Vec<_>>();
                let second_vec = Vec::new();
                let (second, _) = simple_rec(second_vec, second.as_slice());
                let first = PacketData::List(first);
                let second = PacketData::List(second);

                if ordered(&first, &second) {
                    Some(index + 1)
                } else {
                    None
                }
            })
            .sum()
    }

    fn part2(input: &str) -> usize {
        let mut packets: Vec<_> = input
            .split_whitespace()
            .map(|packet_str| {
                let vec = Vec::new();
                let tokens = tokenize(packet_str);
                let tokens: Vec<_> = tokens.iter().skip(1).map(String::as_str).collect();
                PacketData::List(simple_rec(vec, tokens.as_slice()).0)
            })
            .collect();
        let v_two = vec![PacketData::List(vec![PacketData::Int(2)])];
        packets.push(PacketData::List(v_two.clone()));
        let p_compare_two = PacketData::List(v_two);
        let v_six = vec![PacketData::List(vec![PacketData::Int(6)])];
        packets.push(PacketData::List(v_six.clone()));
        let p_compare_six = PacketData::List(v_six);
        packets.sort_by(|p1, p2| in_place_ordering(Ordering::Equal, p1, p2));
        let (two_index, _) = packets
            .iter()
            .enumerate()
            .find(|p| Ordering::Equal == in_place_ordering(Ordering::Equal, p.1, &p_compare_two))
            .unwrap();
        let (six_index, _) = packets
            .iter()
            .enumerate()
            .find(|p| Ordering::Equal == in_place_ordering(Ordering::Equal, p.1, &p_compare_six))
            .unwrap();
        (two_index + 1) * (six_index + 1)
    }
    match part {
        Part::One => Solution::UNumber(part1(input)),
        Part::Two => Solution::UNumber(part2(input)),
    }
}

/// passes problem input to solver for the given day
fn main() -> Result<(), Box<dyn Error>> {
    let days = [
        day1, day2, day3, day4, day5, day6, day7, day8, day9, day10, day11, day12, day13,
    ];
    let today = 12;
    let prod_or_test = "prod";
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        let day = days[today - 1];
        let contents = fs::read_to_string(format!("./input/day{}.{}", today, prod_or_test))
            .expect("where's the input file? didn't find it at './input'");
        let part1 = day(&contents, Part::One);
        let part2 = day(&contents, Part::Two);
        Ok(println!(
            "Solutions:\nPart 1:\n{}\nPart 2:\n{}",
            part1, part2
        ))
    } else {
        let day_index = args[1].parse::<usize>()?;
        let day = days[day_index - 1];
        let part = args[2].parse::<usize>()?.try_into()?;
        let contents = fs::read_to_string(format!("./input/day{}.{}", day_index, prod_or_test))
            .expect("where's the input file? didn't find it at './input'");
        let solution = day(&contents, part);
        Ok(println!("Solution:\n{}", solution))
    }
}
