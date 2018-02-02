//! Functions for safely converting between integer types

use std::{u8, u16, u32, u64, i8, i16, i32, f32};

#[inline]
pub fn into_i8(val: i64) -> Option<i8> {
    if val >= (i8::MIN as i64) && val <= (i8::MAX as i64) {
        Some(val as i8)
    } else {
        None
    }
}

#[inline]
pub fn into_i16(val: i64) -> Option<i16> {
    if val >= (i16::MIN as i64) && val <= (i16::MAX as i64) {
        Some(val as i16)
    } else {
        None
    }
}

#[inline]
pub fn into_i32(val: i64) -> Option<i32> {
    if val >= (i32::MIN as i64) && val <= (i32::MAX as i64) {
        Some(val as i32)
    } else {
        None
    }
}

#[inline]
pub fn into_i64(val: i64) -> Option<i64> {
    Some(val)
}


#[inline]
pub fn into_u8(val: i64) -> Option<u8> {
    if val >= (u8::MIN as i64) && val <= (u8::MAX as i64) {
        Some(val as u8)
    } else {
        None
    }
}

#[inline]
pub fn into_u16(val: i64) -> Option<u16> {
    if val >= (u16::MIN as i64) && val <= (u16::MAX as i64) {
        Some(val as u16)
    } else {
        None
    }
}

#[inline]
pub fn into_u32(val: i64) -> Option<u32> {
    if val >= (u32::MIN as i64) && val <= (u32::MAX as i64) {
        Some(val as u32)
    } else {
        None
    }
}

#[inline]
pub fn into_u64(val: i64) -> Option<u64> {
    Some(val as u64)
}


#[inline]
pub fn into_f32(val: f64) -> Option<f32> {
    Some(val as f32) // FIXME: actually do a width check
}

#[inline]
pub fn into_f64(val: f64) -> Option<f64> {
    Some(val)
}