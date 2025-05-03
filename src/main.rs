/*
 * This is a tool for printing out the date in the Coptic calendar.
 * Known bugs: it accepts invalid Gregorian dates
 *             it probably only works for 1901 -> 2099
 * Author: Martin Keegan (OCaml version)
 * Rust translation: Claude
 * Licence: Apache 2.0
 */

use chrono::{Local, NaiveDate, Datelike};
use std::env;
use std::fmt;

// Modified Julian Day calculation
fn date_to_mjd(date: NaiveDate) -> i32 {
    // MJD calculation formula
    let year = date.year();
    let month = date.month() as i32;
    let day = date.day() as i32;
    
    let a = (14 - month) / 12;
    let y = year + 4800 - a;
    let m = month + 12 * a - 3;
    
    day + (153 * m + 2) / 5 + 365 * y + y / 4 - y / 100 + y / 400 - 32045 - 2400001
}

struct CopticDate {
    date: NaiveDate,
}

impl CopticDate {
    fn from_date(date: NaiveDate) -> Self {
        CopticDate { date }
    }
    
    fn day_in_cycle(&self) -> i32 {
        (date_to_mjd(self.date) - 297) % 1461
    }
    
    fn day_of(&self) -> i32 {
        let dic = self.day_in_cycle();
        if dic == 0 {
            6 // leap day
        } else {
            ((dic - 1) % 365) % 30 + 1
        }
    }
    
    fn month_of(&self) -> i32 {
        let dic = self.day_in_cycle();
        if dic == 0 {
            12 // intercalary month
        } else {
            ((dic - 1) % 365) / 30
        }
    }
    
    fn month_name_of(month: i32) -> &'static str {
        match month {
            0 => "Thout",
            1 => "Paopi",
            2 => "Hathor",
            3 => "Koiak",
            4 => "Tobi",
            5 => "Meshir",
            6 => "Paremhat",
            7 => "Parmouti",
            8 => "Pashons",
            9 => "Paoni",
            10 => "Epip",
            11 => "Mesori",
            12 => "Pi Kogi Enavot",
            _ => panic!("Invalid Coptic month")
        }
    }
}

impl fmt::Display for CopticDate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let day = self.day_of();
        let month = Self::month_name_of(self.month_of());
        write!(f, "{} {}", day, month)
    }
}

fn usage() {
    println!("Usage: adate [YYYY-MM-DD]");
}

fn version() {
    println!("{}", env!("CARGO_PKG_VERSION", "0.1.0"));
}

fn handle(date: NaiveDate) {
    let coptic = CopticDate::from_date(date);
    println!("{}", coptic);
}

fn specific(date_str: &str) -> Result<(), String> {
    match NaiveDate::parse_from_str(date_str, "%Y-%m-%d") {
        Ok(date) => {
            handle(date);
            Ok(())
        },
        Err(e) => Err(format!("Error parsing date: {}", e))
    }
}

fn default() {
    let today = Local::now().date_naive();
    handle(today);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    
    match args.len() {
        2 => {
            match args[1].as_str() {
                "--cal-version" => version(),
                "--help" => usage(),
                date_str => {
                    if let Err(e) = specific(date_str) {
                        eprintln!("{}", e);
                        usage();
                    }
                }
            }
        },
        1 => default(),
        _ => usage(),
    }
}
