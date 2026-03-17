use std::fmt::Display;

#[cfg(feature = "time")]
use std::time::SystemTime;

#[cfg(feature = "time")]
use chrono::{Datelike, TimeZone, Timelike, Utc};
use zerocopy::{FromBytes, Immutable, IntoBytes, KnownLayout, big_endian::I16};

#[cfg(feature = "time")]
use crate::FileSystemError;

/// Date structure used to represent dates within the CBFS file system
#[repr(C)]
#[repr(packed)]
#[derive(
    Debug, Default, Clone, Copy, FromBytes, IntoBytes, KnownLayout, Immutable, Eq, PartialEq,
)]
pub struct Date {
    /// The current year
    pub year: I16,
    /// The current month, starting at 1
    pub month: u8,
    /// The current day, starting at 1
    pub day: u8,
}

/// Time structure used to represent a time stamp within the CBFS file system
#[repr(C)]
#[repr(packed)]
#[derive(
    Debug, Default, Clone, Copy, FromBytes, IntoBytes, KnownLayout, Immutable, Eq, PartialEq,
)]
pub struct Time {
    /// The current hour
    pub hour: u8,
    /// The current minute
    pub minute: u8,
    /// The current second
    pub second: u8,
    /// The current hundredths of a second
    pub hundredths: u8,
}

#[repr(C)]
#[repr(packed)]
#[derive(
    Debug, Default, Clone, Copy, FromBytes, IntoBytes, KnownLayout, Immutable, Eq, PartialEq,
)]
pub struct DateTime {
    pub date: Date,
    pub time: Time,
}

#[cfg(feature = "time")]
impl From<chrono::DateTime<Utc>> for DateTime {
    fn from(value: chrono::DateTime<Utc>) -> Self {
        Self {
            date: Date {
                year: I16::new(value.year() as i16),
                month: value.month() as u8,
                day: value.day() as u8,
            },
            time: Time {
                hour: value.hour() as u8,
                minute: value.minute() as u8,
                second: value.second() as u8,
                hundredths: (value.timestamp_millis() / 100) as u8,
            },
        }
    }
}

#[cfg(feature = "time")]
impl From<SystemTime> for DateTime {
    fn from(value: SystemTime) -> Self {
        let dt = chrono::DateTime::<Utc>::from(value);
        dt.into()
    }
}

#[cfg(feature = "time")]
impl TryFrom<DateTime> for SystemTime {
    type Error = FileSystemError;

    fn try_from(value: DateTime) -> Result<Self, Self::Error> {
        let res = chrono::DateTime::<Utc>::try_from(value)?;
        Ok(res.into())
    }
}

#[cfg(feature = "time")]
impl TryFrom<DateTime> for chrono::DateTime<Utc> {
    type Error = FileSystemError;

    fn try_from(value: DateTime) -> Result<Self, Self::Error> {
        chrono::NaiveDate::from_ymd_opt(
            value.date.year.get() as i32,
            value.date.month as u32,
            value.date.day as u32,
        )
        .and_then(|x| {
            x.and_hms_opt(
                value.time.hour as u32,
                value.time.minute as u32,
                value.time.second as u32,
            )
        })
        .map_or(Err(FileSystemError::InvalidDateTime), |x| Ok(x.and_utc()))
    }
}

#[cfg(feature = "time")]
impl DateTime {
    pub fn to_posix_millis(&self) -> Result<i64, FileSystemError> {
        let dt: chrono::DateTime<Utc> = (*self).try_into()?;
        Ok(dt.timestamp_millis())
    }

    pub fn from_posix_millis(millis: i64) -> Result<Self, FileSystemError> {
        match Utc.timestamp_millis_opt(millis) {
            chrono::offset::LocalResult::Single(dt) => Ok(dt.into()),
            _ => Err(FileSystemError::InvalidDateTime),
        }
    }
}

impl Display for DateTime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}/{}/{} {}:{}:{}",
            self.date.year,
            self.date.month,
            self.date.day,
            self.time.hour,
            self.time.minute,
            self.time.second
        )
    }
}
