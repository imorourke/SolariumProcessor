use std::fmt::Display;

#[cfg(feature = "time")]
use std::time::SystemTime;

#[cfg(feature = "time")]
use chrono::{DateTime, Datelike, Timelike, Utc};
use zerocopy::{FromBytes, Immutable, IntoBytes, KnownLayout, big_endian::I16};

use crate::CbfsError;

#[repr(C)]
#[repr(packed)]
#[derive(Debug, Default, Clone, Copy, FromBytes, IntoBytes, KnownLayout, Immutable)]
pub struct CbDate {
    pub year: I16,
    pub month: u8,
    pub day: u8,
}

#[repr(C)]
#[repr(packed)]
#[derive(Debug, Default, Clone, Copy, FromBytes, IntoBytes, KnownLayout, Immutable)]
pub struct CbTime {
    pub hour: u8,
    pub minute: u8,
    pub second: u8,
    pub hundredths: u8,
}

#[repr(C)]
#[repr(packed)]
#[derive(Debug, Default, Clone, Copy, FromBytes, IntoBytes, KnownLayout, Immutable)]
pub struct CbDateTime {
    date: CbDate,
    time: CbTime,
}

#[cfg(feature = "time")]
impl From<DateTime<Utc>> for CbDateTime {
    fn from(value: DateTime<Utc>) -> Self {
        Self {
            date: CbDate {
                year: I16::new(value.year() as i16),
                month: value.month() as u8,
                day: value.day() as u8,
            },
            time: CbTime {
                hour: value.hour() as u8,
                minute: value.minute() as u8,
                second: value.second() as u8,
                hundredths: (value.timestamp_millis() / 100) as u8,
            },
        }
    }
}

#[cfg(feature = "time")]
impl From<SystemTime> for CbDateTime {
    fn from(value: SystemTime) -> Self {
        let dt = DateTime::<Utc>::from(value);
        dt.into()
    }
}

#[cfg(feature = "time")]
impl TryFrom<CbDateTime> for SystemTime {
    type Error = CbfsError;

    fn try_from(value: CbDateTime) -> Result<Self, Self::Error> {
        let res = DateTime::<Utc>::try_from(value)?;
        Ok(res.into())
    }
}

#[cfg(feature = "time")]
impl TryFrom<CbDateTime> for DateTime<Utc> {
    type Error = CbfsError;

    fn try_from(value: CbDateTime) -> Result<Self, Self::Error> {
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
        .map_or(Err(CbfsError::InvalidDateTime), |x| Ok(x.and_utc()))
    }
}

impl Display for CbDateTime {
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
