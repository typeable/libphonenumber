#ifndef c_phonenumberutil_h
#define c_phonenumberutil_h

#include <stddef.h>

#include "c_phonenumber.h"

// Copied from PhoneNumberUtil::PhoneNumberFormat
// is_same_enum verifies that these values didn't change
enum phone_number_format {
	E164,
	INTERNATIONAL,
	NATIONAL,
	RFC3966,
};

// Copied from PhoneNumberUtil::PhoneNumberType
// is_same_enum verifies that these values didn't change
enum phone_number_type
{
	FIXED_LINE,
	MOBILE,
	FIXED_LINE_OR_MOBILE,
	TOLL_FREE,
	PREMIUM_RATE,
	SHARED_COST,
	VOIP,
	PERSONAL_NUMBER,
	PAGER,
	UAN,
	VOICEMAIL,
	UNKNOWN,
};

// Copied from PhoneNumberUtil::MatchType
// is_same_enum verifies that these values didn't change
enum match_type {
	INVALID_NUMBER,
	NO_MATCH,
	SHORT_NSN_MATCH,
	NSN_MATCH,
	EXACT_MATCH,
};

// Copied from PhoneNumberUtil::ErrorType
// is_same_enum verifies that these values didn't change
enum error_type
{
	NO_PARSING_ERROR,
	INVALID_COUNTRY_CODE_ERROR,
	NOT_A_NUMBER,
	TOO_SHORT_AFTER_IDD,
	TOO_SHORT_NSN,
	TOO_LONG_NSN,
};

// Copied from PhoneNumberUtil::ValidationResult
// is_same_enum verifies that these values didn't change
enum validation_result
{
	IS_POSSIBLE,
	IS_POSSIBLE_LOCAL_ONLY,
	INVALID_COUNTRY_CODE,
	TOO_SHORT,
	INVALID_LENGTH,
	TOO_LONG,
};

// A string allocated by malloc, might not include the null terminator.
// It is haskell's responsibility to free it.
typedef struct
{
	char *string;
	size_t string_size;
} c_string;

// In the functions below, if an output parameter is a string, we malloc a
// buffer and write its pointer and size into the given c_string *.
//
// If an input parameter is a string, we "unpack" the c_string into a pair of
// char const * + size_t, because c2hs cannot natively pass structs by value.
//
// If the output is a list of T, we malloc a buffer and write its pointer into
// the given T **, and its size (in T's) into the given size_t *.
// If the output is a list of strings, we do the same as if T=c_string.
//
// If the output is a PhoneNumber, we either return a PhoneNumber *, or write
// it into a provided PhoneNumber **. In both cases the PhoneNumber object is
// allocated by `new` and managed by C++ RAII. In all cases it is haskell's
// responsibility to call its finalizer (c_phone_number_free);
void c_phone_number_util_get_supported_regions(c_string **, size_t *);
void c_phone_number_util_get_supported_global_network_calling_codes(int **, size_t *);
void c_phone_number_util_get_supported_calling_codes(int **, size_t *);
void c_phone_number_util_get_supported_types_for_region(char const *, size_t, enum phone_number_type **, size_t *);
void c_phone_number_util_get_supported_types_for_non_geo_entity(int, enum phone_number_type **, size_t *);
int c_phone_number_util_is_alpha_number(char const *, size_t);
void c_phone_number_util_convert_alpha_characters_in_number(char const *, size_t, c_string *);
void c_phone_number_util_normalize_digits_only(char const *, size_t, c_string *);
void c_phone_number_util_normalize_dialable_chars_only(char const *, size_t, c_string *);
void c_phone_number_util_get_national_significant_number(struct CxxPhoneNumber const *, c_string *);
void c_phone_number_util_get_country_mobile_token(int, c_string *);
void c_phone_number_util_format(struct CxxPhoneNumber const *, enum phone_number_format, c_string *);
void c_phone_number_util_format_national_number_with_carrier_code(struct CxxPhoneNumber const *, char const *, size_t, c_string *);
void c_phone_number_util_format_national_number_with_preferred_carrier_code(struct CxxPhoneNumber const *, char const *, size_t, c_string *);
void c_phone_number_util_format_number_for_mobile_dialing(struct CxxPhoneNumber const *, char const *, size_t, int, c_string *);
void c_phone_number_util_format_out_of_country_calling_number(struct CxxPhoneNumber const *, char const *, size_t, c_string *);
void c_phone_number_util_format_in_original_format(struct CxxPhoneNumber const *, char const *, size_t, c_string *);
void c_phone_number_util_format_out_of_country_keeping_alpha_chars(struct CxxPhoneNumber const *, char const *, size_t, c_string *);
// /!\ May return NULL
struct CxxPhoneNumber *c_phone_number_util_truncate_too_long_number(struct CxxPhoneNumber const *);
enum phone_number_type c_phone_number_util_get_number_type(struct CxxPhoneNumber const *);
int c_phone_number_util_is_valid_number(struct CxxPhoneNumber const *);
int c_phone_number_util_is_valid_number_for_region(struct CxxPhoneNumber const *, char const *, size_t);
void c_phone_number_util_get_region_code_for_number(struct CxxPhoneNumber const *, c_string *);
int c_phone_number_util_get_country_code_for_region(char const *, size_t);
void c_phone_number_util_get_region_code_for_country_code(int, c_string *);
void c_phone_number_util_get_region_codes_for_country_calling_code(int, c_string **, size_t *);
int c_phone_number_util_is_nanpa_country(char const *, size_t);
void c_phone_number_util_get_ndd_prefix_for_region(char const *, size_t, int, c_string *);
enum validation_result c_phone_number_util_is_possible_number_for_type_with_reason(struct CxxPhoneNumber const *, enum phone_number_type);
int c_phone_number_util_can_be_internationally_dialed(struct CxxPhoneNumber const *);
int c_phone_number_util_is_number_geographical_1(struct CxxPhoneNumber const *);
int c_phone_number_util_is_number_geographical_2(enum phone_number_type, int);
enum error_type c_phone_number_util_parse(char const *, size_t, char const *, size_t, struct CxxPhoneNumber **);
enum error_type c_phone_number_util_parse_and_keep_raw_input(char const *, size_t, char const *, size_t, struct CxxPhoneNumber **);
enum match_type c_phone_number_util_is_number_match(struct CxxPhoneNumber const *, struct CxxPhoneNumber const *);
enum match_type c_phone_number_util_is_number_match_with_two_strings(char const *, size_t, char const *, size_t);
enum match_type c_phone_number_util_is_number_match_with_one_string(struct CxxPhoneNumber const *, char const *, size_t);

#endif
