use std::collections::HashMap;
use std::fmt::{Display, Formatter, Write};
use std::fs::File;

use itertools::Itertools;

use crate::{Part, Solution};

#[derive(Debug)]
enum Shape {
    HorizontalLine,
    Cross,
    ReflectedL,
    VerticalLine,
    Square,
}

impl Shape {
    fn spaces_below(
        &self,
        lower_left_corner_of_bounding_box: Position,
    ) -> impl Iterator<Item = Position> {
        let (box_x, box_y) = lower_left_corner_of_bounding_box;
        match self {
            Shape::HorizontalLine => vec![
                (box_x, box_y - 1),
                (box_x + 1, box_y - 1),
                (box_x + 2, box_y - 1),
                (box_x + 3, box_y - 1),
            ],
            Shape::Cross => {
                vec![(box_x, box_y), (box_x + 1, box_y - 1), (box_x + 2, box_y)]
            }
            Shape::ReflectedL => vec![
                (box_x, box_y - 1),
                (box_x + 1, box_y - 1),
                (box_x + 2, box_y - 1),
            ],
            Shape::VerticalLine => vec![(box_x, box_y - 1)],
            Shape::Square => vec![(box_x, box_y - 1), (box_x + 1, box_y - 1)],
        }
        .into_iter()
    }
}
impl Shape {
    fn occupied_spaces(
        &self,
        lower_left_corner_of_bounding_box: Position,
    ) -> impl Iterator<Item = Position> {
        let (box_x, box_y) = lower_left_corner_of_bounding_box;
        match self {
            Shape::HorizontalLine => vec![
                (box_x, box_y),
                (box_x + 1, box_y),
                (box_x + 2, box_y),
                (box_x + 3, box_y),
            ],
            Shape::Cross => vec![
                (box_x, box_y + 1),
                (box_x + 1, box_y),
                (box_x + 1, box_y + 1),
                (box_x + 1, box_y + 2),
                (box_x + 2, box_y + 1),
            ],
            Shape::ReflectedL => vec![
                (box_x, box_y),
                (box_x + 1, box_y),
                (box_x + 2, box_y),
                (box_x + 2, box_y + 1),
                (box_x + 2, box_y + 2),
            ],
            Shape::VerticalLine => vec![
                (box_x, box_y),
                (box_x, box_y + 1),
                (box_x, box_y + 2),
                (box_x, box_y + 3),
            ],
            Shape::Square => vec![
                (box_x, box_y),
                (box_x, box_y + 1),
                (box_x + 1, box_y),
                (box_x + 1, box_y + 1),
            ],
        }
        .into_iter()
    }
}

const SHAPE_ORDER: [Shape; 5] = [
    Shape::HorizontalLine,
    Shape::Cross,
    Shape::ReflectedL,
    Shape::VerticalLine,
    Shape::Square,
];

#[derive(Debug)]
enum Jet {
    Left,
    Right,
}

impl Display for Jet {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_char(match self {
            Jet::Left => '<',
            Jet::Right => '>',
        })
    }
}

const CHAMBER_WIDTH: u8 = 7;
const ROCK_STARTING_LEFT_OFFSET: u32 = 2;
const ROCK_STARTING_HEIGHT_OFFSET: u32 = 3;

fn parse_jets(input: &str) -> Vec<Jet> {
    input
        .chars()
        .flat_map(|c| match c {
            '<' => Some(Jet::Left),
            '>' => Some(Jet::Right),
            _ => None,
        })
        .collect_vec()
}

type Position = (i32, i32);

fn write_map(
    mut output_file: File,
    map: &HashMap<Position, bool>,
    falling_rock: (&Position, &Shape),
    min_x: usize,
    min_y: usize,
    max_x: usize,
    max_y: usize,
) {
    use std::io::Write;
    let width = max_x - min_x + 1;
    let height = max_y - min_y + 1;
    let (bottom_left_corner, shape) = falling_rock;
    output_file
        .write_all(format!("P3\n{} {}\n255\n", width, height).as_bytes())
        .unwrap();

    for y in (min_y..=(max_y)).rev() {
        let line = (min_x..=(max_x))
            .map(|x| {
                if x == 0 || x == 8 || y == 0 {
                    "0 0 110"
                } else if shape
                    .occupied_spaces(*bottom_left_corner)
                    .contains(&(x as i32, y as i32))
                {
                    "242 211 191"
                } else {
                    match map.get(&(x as i32, y as i32)) {
                        None => "51 51 51",
                        Some(is_rock) => {
                            if *is_rock {
                                "112 128 144"
                            } else {
                                "0 0 0"
                            }
                        }
                    }
                }
            })
            .collect_vec();
        output_file
            .write_all((line.join(" ") + "\n").as_bytes())
            .unwrap();
    }
}

fn part1(input: &str) -> u32 {
    const NUMBER_TO_FALL: usize = 2022;
    // const NUMBER_TO_FALL: usize = 200;
    let jets = parse_jets(input);
    let mut forever_jets = jets.iter().cycle();
    let rocks_to_fall = SHAPE_ORDER.iter().cycle().take(NUMBER_TO_FALL);

    let mut tunnel_map = HashMap::<Position, bool>::new();
    // fill in floor
    for x in 1..=(CHAMBER_WIDTH) {
        tunnel_map.insert((x as i32, 0), true);
    }

    let mut frame_index = 0;
    for rock in rocks_to_fall {
        println!("\n=#= Next Rock: {:?} =#=", rock);
        let top_of_stack = tunnel_map
            .iter()
            .filter_map(|((_, y), is_rock)| if *is_rock { Some(y) } else { None })
            .max()
            .unwrap();
        let mut rock_bounding_box_bottom_left_position = (
            ROCK_STARTING_LEFT_OFFSET as i32 + 1,
            top_of_stack + ROCK_STARTING_HEIGHT_OFFSET as i32 + 1,
        );
        loop {
            let output_file =
                File::create(format!("./output/frame{}.ppm", frame_index).as_str()).unwrap();
            write_map(
                output_file,
                &tunnel_map,
                (&rock_bounding_box_bottom_left_position, rock),
                0,
                0,
                (CHAMBER_WIDTH + 1) as usize,
                330,
            );
            let next_jet = forever_jets.next().unwrap();
            // println!("=== Next Jet: {} ===", next_jet);
            // println!(
            //     "Before movement from jet: {:?}",
            //     rock_bounding_box_bottom_left_position
            // );
            let jet_x_movement = match next_jet {
                Jet::Left => -1,
                Jet::Right => 1,
            };
            let (current_x, current_y) = rock_bounding_box_bottom_left_position;
            if rock
                .occupied_spaces((current_x + jet_x_movement, current_y))
                .all(|position| {
                    position.0 <= CHAMBER_WIDTH as i32
                        && position.0 > 0
                        && !*tunnel_map.get(&position).unwrap_or(&false)
                })
            {
                rock_bounding_box_bottom_left_position.0 += jet_x_movement;
            }
            // println!(
            //     "After movement from jet: {:?}",
            //     rock_bounding_box_bottom_left_position
            // );
            if rock
                .spaces_below(rock_bounding_box_bottom_left_position)
                .any(|position| *tunnel_map.get(&position).unwrap_or(&false))
            {
                break;
            }
            rock_bounding_box_bottom_left_position.1 -= 1;
            frame_index += 1;
        }
        for position in rock.occupied_spaces(rock_bounding_box_bottom_left_position) {
            tunnel_map.insert(position, true);
        }
    }
    *tunnel_map.keys().map(|(_x, y)| y).max().unwrap() as u32
}

fn part2(input: &str) -> u64 {
    type Position = (i32, u64);

    impl Shape {
        fn occupied_spaces_packed(
            &self,
            lower_left_corner_of_bounding_box: Position,
        ) -> impl Iterator<Item = Position> {
            let (box_x, box_y) = lower_left_corner_of_bounding_box;
            match self {
                Shape::HorizontalLine => vec![
                    (box_x, box_y),
                    (box_x + 1, box_y),
                    (box_x + 2, box_y),
                    (box_x + 3, box_y),
                ],
                Shape::Cross => vec![
                    (box_x, box_y + 1),
                    (box_x + 1, box_y),
                    (box_x + 1, box_y + 1),
                    (box_x + 1, box_y + 2),
                    (box_x + 2, box_y + 1),
                ],
                Shape::ReflectedL => vec![
                    (box_x, box_y),
                    (box_x + 1, box_y),
                    (box_x + 2, box_y),
                    (box_x + 2, box_y + 1),
                    (box_x + 2, box_y + 2),
                ],
                Shape::VerticalLine => vec![
                    (box_x, box_y),
                    (box_x, box_y + 1),
                    (box_x, box_y + 2),
                    (box_x, box_y + 3),
                ],
                Shape::Square => vec![
                    (box_x, box_y),
                    (box_x, box_y + 1),
                    (box_x + 1, box_y),
                    (box_x + 1, box_y + 1),
                ],
            }
            .into_iter()
        }
    }
    impl Shape {
        fn spaces_below_packed(
            &self,
            lower_left_corner_of_bounding_box: (i32, u64),
        ) -> impl Iterator<Item = Position> {
            let (box_x, box_y) = lower_left_corner_of_bounding_box;
            match self {
                Shape::HorizontalLine => vec![
                    (box_x, box_y - 1),
                    (box_x + 1, box_y - 1),
                    (box_x + 2, box_y - 1),
                    (box_x + 3, box_y - 1),
                ],
                Shape::Cross => {
                    vec![(box_x, box_y), (box_x + 1, box_y - 1), (box_x + 2, box_y)]
                }
                Shape::ReflectedL => vec![
                    (box_x, box_y - 1),
                    (box_x + 1, box_y - 1),
                    (box_x + 2, box_y - 1),
                ],
                Shape::VerticalLine => vec![(box_x, box_y - 1)],
                Shape::Square => vec![(box_x, box_y - 1), (box_x + 1, box_y - 1)],
            }
            .into_iter()
        }
    }
    const NUMBER_TO_FALL: u64 = 1_000_000_000_000;
    let jets = parse_jets(input);
    let mut forever_jets = jets.iter().cycle();
    let rocks_to_fall = SHAPE_ORDER
        .iter()
        .cycle()
        .zip(0u64..)
        .take_while(|(_, index)| *index <= NUMBER_TO_FALL)
        .map(|(rock, _)| rock);

    let mut highest_complete_line = 0u64;

    #[derive(Clone, Copy)]
    struct Line(u8);
    impl Line {
        pub fn at(&self, index: u8) -> bool {
            (self.0 & (0b1 << index)) > 0
        }
    }
    /*
    assert!(Line(0b01111111).at(0));
    assert!(Line(0b01111111).at(1));
    assert!(Line(0b01111111).at(2));
    assert!(Line(0b01111111).at(3));
    assert!(Line(0b01111111).at(4));
    assert!(Line(0b01111111).at(5));
    assert!(Line(0b01111111).at(6));

    assert!(!Line(0b10000000).at(0));
    assert!(!Line(0b10000000).at(1));
    assert!(!Line(0b10000000).at(2));
    assert!(!Line(0b10000000).at(3));
    assert!(!Line(0b10000000).at(4));
    assert!(!Line(0b10000000).at(5));
    assert!(!Line(0b10000000).at(6));
    */
    impl Line {
        pub fn set(&mut self, index: u8) {
            self.0 |= 0b1 << index
        }
    }

    impl Display for Line {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            f.write_char('|')?;
            for i in 0..7 {
                f.write_char(if self.at(i) { '#' } else { '.' })?;
            }
            f.write_char('|')
        }
    }

    impl std::fmt::Debug for Line {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            f.write_str(format!("{}\t({:#010b})", self, self.0).as_str())
        }
    }

    #[derive(Clone)]
    struct Map {
        lines: std::collections::VecDeque<Line>,
    }
    impl Map {
        pub fn at(&self, position: Position, vertical_offset: u64) -> bool {
            if (position.0 < 0)
                || (position.0 >= CHAMBER_WIDTH as i32)
                || (position.1 <= vertical_offset)
            {
                true
            } else if position.1 > (vertical_offset + self.lines.len() as u64) {
                false
            } else {
                self.lines[(position.1 - vertical_offset - 1) as usize].at(position.0 as u8)
            }
        }
    }
    impl Display for Map {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            f.write_str("     |       |\n")?;
            for y in (0..self.lines.len()).rev() {
                // self.lines[y].fmt(f)?;
                f.write_str(format!("{:#02} - {:?}", y + 1, self.lines[y]).as_str())?;
                f.write_char('\n')?;
            }
            f.write_str("00 - +-------+\n")?;
            Ok(())
        }
    }
    impl Map {
        /// Position for insertion *MUST* be already contained by map *OR* be adjacent to map contents
        /// (i.e. ``1 <= insertion_y <= (map_max_y + 1)``. Inserting a position whose y-coordinate is
        /// outside this range is **Undefined Behavior**.
        pub fn insert(&mut self, position: Position, vertical_offset: u64) {
            if self.lines.is_empty() || (position.1 > (vertical_offset + self.lines.len() as u64)) {
                self.lines.push_back(Line(1 << position.0));
            } else {
                self.lines[(position.1 - vertical_offset - 1) as usize].set(position.0 as u8);
            }
        }
    }
    impl Map {
        pub fn cull(&mut self, vertical_offset: u64) -> u64 {
            if let Some(highest_full_line_index) =
                self.lines.iter().position(|line| line.0 == 0b01111111)
            {
                for _ in 0..=highest_full_line_index {
                    self.lines.pop_front();
                }
                println!("culled {} lines", highest_full_line_index);
                vertical_offset + highest_full_line_index as u64
            } else {
                vertical_offset
            }
        }
    }
    let mut tunnel_map = Map {
        lines: std::collections::VecDeque::new(),
    };
    let mut counter = 0u64;
    for rock in rocks_to_fall {
        counter += 1;
        if counter < 40 {
            println!("{}", &tunnel_map);
            println!("Next rock: {:?}", rock);
            dbg!(&tunnel_map.lines.len(), highest_complete_line);
        } else {
            break;
        }
        let top_of_stack = highest_complete_line + tunnel_map.lines.len() as u64;
        let mut rock_bounding_box_bottom_left_position = (
            ROCK_STARTING_LEFT_OFFSET as i32,                  // [-1, 7]
            top_of_stack + ROCK_STARTING_HEIGHT_OFFSET as u64, // [0, ?]
        );

        print!("Jets: ");
        loop {
            let next_jet = forever_jets.next().unwrap();
            print!("{}", &next_jet);
            let jet_x_movement = match next_jet {
                Jet::Left => -1,
                Jet::Right => 1,
            };
            let (current_x, current_y) = rock_bounding_box_bottom_left_position;
            if !rock
                .occupied_spaces_packed((current_x + jet_x_movement, current_y))
                .any(|position| tunnel_map.at(position, highest_complete_line))
            {
                rock_bounding_box_bottom_left_position.0 += jet_x_movement;
            }
            if rock
                .spaces_below_packed(rock_bounding_box_bottom_left_position)
                .any(|position| tunnel_map.at(position, highest_complete_line))
            {
                break;
            }
            rock_bounding_box_bottom_left_position.1 -= 1;
        }
        println!();

        println!(
            "rock stopped at {:?}",
            &rock_bounding_box_bottom_left_position
        );
        println!("New solid tiles:");
        for position in rock
            .occupied_spaces_packed(rock_bounding_box_bottom_left_position)
            // insertion must be on existing y or (highest_y + 1)
            // sort by ascending y-coordinate to maintain ^this^ invariant
            .sorted_by(|position_a, position_b| position_a.1.cmp(&position_b.1))
        {
            print!(" {:?} =>", position);
            tunnel_map.insert(position, highest_complete_line);
            println!(" {:?}", tunnel_map.lines[(position.1 - 1) as usize]);
        }
        highest_complete_line = tunnel_map.cull(highest_complete_line);
    }
    // let mut tunnel_map = HashMap<Position, bool>

    highest_complete_line + (tunnel_map.lines.len() as u64)
}

pub fn day17(input: &str, part: Part) -> Solution {
    match part {
        Part::One => Solution::U32(part1(input)),
        Part::Two => Solution::U64(part2(input)),
    }
}
