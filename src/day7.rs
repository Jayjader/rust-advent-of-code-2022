use std::collections::HashMap;

use crate::{Part, Solution};

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
fn total_size(
    tree: &HashMap<String, Vec<DirEntry<'_>>>,
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
                let path = [dir.as_str(), name].join("/");
                total_size(tree, path, cache)
            }
        })
        .sum();
    cache.insert(dir, size);
    size
}

/// solves problem for day 7
pub fn day7(input: &str, part: Part) -> Solution {
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
        Part::One => Solution::USize(part1(input)),
        Part::Two => Solution::USize(part2(input)),
    }
}
