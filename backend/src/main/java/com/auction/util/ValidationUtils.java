package com.auction.util;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

public class ValidationUtils {

    // DateTimeFormatter for parsing date and time inputs
    private static final DateTimeFormatter DATETIME_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");

    /**
     * Validates that a string is not null or empty.
     * @param value the value to check
     * @param fieldName the name of the field (for error messages)
     * @return error message if invalid, null if valid
     */
    public static String validateRequired(String value, String fieldName) {
        if (value == null || value.trim().isEmpty()) {
            return fieldName + " is required.";
        }
        return null;
    }

    /**
     * Validates and parses a positive double value.
     * @param value the string to parse
     * @param fieldName the name of the field (for error messages)
     * @return the parsed double, or throws exception with error message
     */
    public static double validatePositiveDouble(String value, String fieldName) throws ValidationException {
        try {
            double parsed = Double.parseDouble(value.trim());
            if (parsed < 0) {
                throw new ValidationException(fieldName + " cannot be negative.");
            }
            return parsed;
        } catch (NumberFormatException e) {
            throw new ValidationException("Invalid " + fieldName + ".");
        }
    }

    /**
     * Validates and parses a strictly positive double value (> 0).
     * @param value the string to parse
     * @param fieldName the name of the field (for error messages)
     * @return the parsed double, or throws exception with error message
     */
    public static double validateStrictlyPositiveDouble(String value, String fieldName) throws ValidationException {
        try {
            double parsed = Double.parseDouble(value.trim());
            if (parsed <= 0) {
                throw new ValidationException(fieldName + " must be positive.");
            }
            return parsed;
        } catch (NumberFormatException e) {
            throw new ValidationException("Invalid " + fieldName + ".");
        }
    }

    /**
     * Validates and parses a strictly positive integer value (> 0).
     * @param value the string to parse
     * @param fieldName the name of the field (for error messages)
     * @return the parsed integer, or throws exception with error message
     */
    public static int validatePositiveInteger(String value, String fieldName) throws ValidationException {
        try {
            int parsed = Integer.parseInt(value.trim());
            if (parsed <= 0) {
                throw new ValidationException(fieldName + " must be positive.");
            }
            return parsed;
        } catch (NumberFormatException e) {
            throw new ValidationException("Invalid " + fieldName + ".");
        }
    }

    /**
     * Validates and parses a non-negative integer value (>= 0).
     * @param value the string to parse
     * @param fieldName the name of the field (for error messages)
     * @return the parsed integer, or throws exception with error message
     */
    public static int validateNonNegativeInteger(String value, String fieldName) throws ValidationException {
        try {
            int parsed = Integer.parseInt(value.trim());
            if (parsed < 0) {
                throw new ValidationException(fieldName + " cannot be negative.");
            }
            return parsed;
        } catch (NumberFormatException e) {
            throw new ValidationException("Invalid " + fieldName + ".");
        }
    }

    /**
     * Validates and parses a non-negative double value (>= 0).
     * @param value the string to parse
     * @param fieldName the name of the field (for error messages)
     * @return the parsed double, or throws exception with error message
     */
    public static double validateNonNegativeDouble(String value, String fieldName) throws ValidationException {
        try {
            double parsed = Double.parseDouble(value.trim());
            if (parsed < 0) {
                throw new ValidationException(fieldName + " cannot be negative.");
            }
            return parsed;
        } catch (NumberFormatException e) {
            throw new ValidationException("Invalid " + fieldName + " format.");
        }
    }

    /**
     * Validates and parses a positive BigDecimal value (>= 0).
     * @param value the string to parse
     * @param fieldName the name of the field (for error messages)
     * @return the parsed BigDecimal, or throws exception with error message
     */
    public static BigDecimal validatePositiveBigDecimal(String value, String fieldName) throws ValidationException {
        try {
            BigDecimal parsed = new BigDecimal(value.trim());
            if (parsed.compareTo(BigDecimal.ZERO) < 0) {
                throw new ValidationException(fieldName + " cannot be negative.");
            }
            return parsed;
        } catch (NumberFormatException e) {
            throw new ValidationException("Invalid " + fieldName + " format.");
        }
    }

    /**
     * Validates and parses a strictly positive BigDecimal value (> 0).
     * @param value the string to parse
     * @param fieldName the name of the field (for error messages)
     * @return the parsed BigDecimal, or throws exception with error message
     */
    public static BigDecimal validateStrictlyPositiveBigDecimal(String value, String fieldName) throws ValidationException {
        try {
            BigDecimal parsed = new BigDecimal(value.trim());
            if (parsed.compareTo(BigDecimal.ZERO) <= 0) {
                throw new ValidationException(fieldName + " must be positive.");
            }
            return parsed;
        } catch (NumberFormatException e) {
            throw new ValidationException("Invalid " + fieldName + " format.");
        }
    }

    /**
     * Validates a future date and time (must not be in the past).
     * Assumes the input is already in UTC.
     * @param dateStr the date string (yyyy-MM-dd) in UTC
     * @param timeStr the time string (HH:mm) in UTC
     * @return the parsed LocalDateTime in UTC, or throws exception with error message
     */
    public static LocalDateTime validateFutureDateTime(String dateStr, String timeStr) throws ValidationException {
        try {
            String combined = dateStr + " " + timeStr;
            LocalDateTime dateTime = LocalDateTime.parse(combined, DATETIME_FORMATTER);
            LocalDateTime nowUtc = LocalDateTime.now(ZoneOffset.UTC);
            if (dateTime.isBefore(nowUtc)) {
                throw new ValidationException("Start date and time cannot be in the past.");
            }
            return dateTime;
        } catch (DateTimeParseException e) {
            throw new ValidationException("Invalid date or time format.");
        }
    }

    /**
     * Validates a future date and time with timezone conversion to UTC.
     * @param dateStr the date string (yyyy-MM-dd) in user's local timezone
     * @param timeStr the time string (HH:mm) in user's local timezone
     * @param timezoneOffsetMinutes the timezone offset in minutes (negative for UTC+, positive for UTC-)
     * @return the parsed LocalDateTime in UTC, or throws exception with error message
     */
    public static LocalDateTime validateFutureDateTimeWithTimezone(String dateStr, String timeStr, int timezoneOffsetMinutes) throws ValidationException {
        try {
            String combined = dateStr + " " + timeStr;
            LocalDateTime localDateTime = LocalDateTime.parse(combined, DATETIME_FORMATTER);
            
            // Convert user's local time to UTC
            // timezoneOffsetMinutes is negative for UTC+ (e.g., -60 for UTC+1)
            // We need to subtract the offset to convert to UTC
            LocalDateTime utcDateTime = localDateTime.plusMinutes(timezoneOffsetMinutes);
            
            // Check if the UTC time is in the past
            LocalDateTime nowUtc = LocalDateTime.now(ZoneOffset.UTC);
            if (utcDateTime.isBefore(nowUtc)) {
                throw new ValidationException("Start date and time cannot be in the past.");
            }
            
            return utcDateTime;
        } catch (DateTimeParseException e) {
            throw new ValidationException("Invalid date or time format.");
        }
    }

    /**
     * Custom exception for validation errors.
     */
    public static class ValidationException extends Exception {
        public ValidationException(String message) {
            super(message);
        }
    }
}
