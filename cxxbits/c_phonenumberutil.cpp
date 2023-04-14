#include <cstdlib>
#include <list>
#include <set>
#include <string>
#include <phonenumbers/phonenumberutil.h>

#include "common.h"
extern "C" {
#include "c_phonenumberutil.h"
}

using i18n::phonenumbers::PhoneNumber, i18n::phonenumbers::PhoneNumberUtil;

static PhoneNumberUtil::PhoneNumberFormat marshal_phone_number_format(phone_number_format value)
{
	static_assert(is_same_enum<
			enum_values<phone_number_format,
				E164,
				INTERNATIONAL,
				NATIONAL,
				RFC3966
			>,
			enum_values<PhoneNumberUtil::PhoneNumberFormat,
				PhoneNumberUtil::E164,
				PhoneNumberUtil::INTERNATIONAL,
				PhoneNumberUtil::NATIONAL,
				PhoneNumberUtil::RFC3966
			>
		>::value,
		"PhoneNumberFormat enum changed"
	);
	return static_cast<PhoneNumberUtil::PhoneNumberFormat>(value);
}

static PhoneNumberUtil::PhoneNumberType marshal_phone_number_type(phone_number_type value)
{
	static_assert(is_same_enum<
			enum_values<phone_number_type,
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
				UNKNOWN
			>,
			enum_values<PhoneNumberUtil::PhoneNumberType,
				PhoneNumberUtil::MOBILE,
				PhoneNumberUtil::FIXED_LINE_OR_MOBILE,
				PhoneNumberUtil::TOLL_FREE,
				PhoneNumberUtil::PREMIUM_RATE,
				PhoneNumberUtil::SHARED_COST,
				PhoneNumberUtil::VOIP,
				PhoneNumberUtil::PERSONAL_NUMBER,
				PhoneNumberUtil::PAGER,
				PhoneNumberUtil::UAN,
				PhoneNumberUtil::VOICEMAIL,
				PhoneNumberUtil::UNKNOWN
			>
		>::value,
		"PhoneNumberType enum changed"
	);
	return static_cast<PhoneNumberUtil::PhoneNumberType>(value);
}

static phone_number_type unmarshal_phone_number_type(PhoneNumberUtil::PhoneNumberType value)
{
	static_assert(is_same_enum<
			enum_values<phone_number_type,
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
				UNKNOWN
			>,
			enum_values<PhoneNumberUtil::PhoneNumberType,
				PhoneNumberUtil::MOBILE,
				PhoneNumberUtil::FIXED_LINE_OR_MOBILE,
				PhoneNumberUtil::TOLL_FREE,
				PhoneNumberUtil::PREMIUM_RATE,
				PhoneNumberUtil::SHARED_COST,
				PhoneNumberUtil::VOIP,
				PhoneNumberUtil::PERSONAL_NUMBER,
				PhoneNumberUtil::PAGER,
				PhoneNumberUtil::UAN,
				PhoneNumberUtil::VOICEMAIL,
				PhoneNumberUtil::UNKNOWN
			>
		>::value,
		"PhoneNumberType enum changed"
	);
	return static_cast<phone_number_type>(value);
}

static match_type unmarshal_match_type(PhoneNumberUtil::MatchType value)
{
	static_assert(is_same_enum<
			enum_values<match_type,
				INVALID_NUMBER,
				NO_MATCH,
				SHORT_NSN_MATCH,
				NSN_MATCH,
				EXACT_MATCH
			>,
			enum_values<PhoneNumberUtil::MatchType,
				PhoneNumberUtil::INVALID_NUMBER,
				PhoneNumberUtil::NO_MATCH,
				PhoneNumberUtil::SHORT_NSN_MATCH,
				PhoneNumberUtil::NSN_MATCH,
				PhoneNumberUtil::EXACT_MATCH
			>
		>::value,
		"MatchType enum changed"
	);
	return static_cast<match_type>(value);
}

static error_type unmarshal_error_type(PhoneNumberUtil::ErrorType value)
{
	static_assert(is_same_enum<
			enum_values<error_type,
				NO_PARSING_ERROR,
				INVALID_COUNTRY_CODE_ERROR,
				NOT_A_NUMBER,
				TOO_SHORT_AFTER_IDD,
				TOO_SHORT_NSN,
				TOO_LONG_NSN
			>,
			enum_values<PhoneNumberUtil::ErrorType,
				PhoneNumberUtil::NO_PARSING_ERROR,
				PhoneNumberUtil::INVALID_COUNTRY_CODE_ERROR,
				PhoneNumberUtil::NOT_A_NUMBER,
				PhoneNumberUtil::TOO_SHORT_AFTER_IDD,
				PhoneNumberUtil::TOO_SHORT_NSN,
				PhoneNumberUtil::TOO_LONG_NSN
			>
		>::value,
		"ErrorType enum changed"
	);
	return static_cast<error_type>(value);
}

static validation_result unmarshal_validation_result(PhoneNumberUtil::ValidationResult value)
{
	static_assert(is_same_enum<
			enum_values<validation_result,
				IS_POSSIBLE,
				IS_POSSIBLE_LOCAL_ONLY,
				INVALID_COUNTRY_CODE,
				TOO_SHORT,
				INVALID_LENGTH,
				TOO_LONG
			>,
			enum_values<PhoneNumberUtil::ValidationResult,
				PhoneNumberUtil::IS_POSSIBLE,
				PhoneNumberUtil::IS_POSSIBLE_LOCAL_ONLY,
				PhoneNumberUtil::INVALID_COUNTRY_CODE,
				PhoneNumberUtil::TOO_SHORT,
				PhoneNumberUtil::INVALID_LENGTH,
				PhoneNumberUtil::TOO_LONG
			>
		>::value,
		"ValidationResult enum changed"
	);
	return static_cast<validation_result>(value);
}

static void unmarshal_c_string(std::string const &string, c_string &c_string)
{
	unmarshal_string_size(string, c_string.string, c_string.string_size);
}

template<typename T>
static void unmarshal_int_list(T ints, int *&c_ints, size_t &c_ints_size)
{
	c_ints = static_cast<int *>(calloc(ints.size(), sizeof(int)));
	c_ints_size = ints.size();
	size_t i = 0;
	for (auto x : ints)
		c_ints[i++] = x;
}

template<typename T>
static void unmarshal_type_list(T types, phone_number_type *&c_types, size_t &c_types_size)
{
	c_types = static_cast<phone_number_type *>(calloc(types.size(), sizeof(phone_number_type)));
	c_types_size = types.size();
	size_t i = 0;
	for (auto t : types)
		c_types[i++] = unmarshal_phone_number_type(t);
}

template<typename T>
static void unmarshal_string_list(T strings, c_string *&c_strings, size_t &c_strings_size)
{
	c_strings = static_cast<c_string *>(calloc(strings.size(), sizeof *c_strings));
	c_strings_size = strings.size();
	size_t i = 0;
	for (auto const &string : strings)
	{
		unmarshal_c_string(string, c_strings[i]);
		i++;
	}
}

extern "C" void c_phone_number_util_get_supported_regions(
	c_string **c_regions,
	size_t *c_regions_size
)
{
	std::set<std::string> regions;
	PhoneNumberUtil::GetInstance()->GetSupportedRegions(&regions);
	unmarshal_string_list(regions, *c_regions, *c_regions_size);
}

extern "C" void c_phone_number_util_get_supported_global_network_calling_codes(
	int **c_calling_codes,
	size_t *c_calling_codes_size
)
{
	std::set<int> calling_codes;
	PhoneNumberUtil::GetInstance()->GetSupportedGlobalNetworkCallingCodes(&calling_codes);
	unmarshal_int_list(calling_codes, *c_calling_codes, *c_calling_codes_size);
}

extern "C" void c_phone_number_util_get_supported_calling_codes(
	int **c_calling_codes,
	size_t *c_calling_codes_size
)
{
	std::set<int> calling_codes;
	PhoneNumberUtil::GetInstance()->GetSupportedCallingCodes(&calling_codes);
	unmarshal_int_list(calling_codes, *c_calling_codes, *c_calling_codes_size);
}

extern "C" void c_phone_number_util_get_supported_types_for_region(
	char const *c_region,
	size_t c_region_size,
	phone_number_type **c_types,
	size_t *c_types_size
)
{
	std::set<PhoneNumberUtil::PhoneNumberType> types;
	std::string const region(c_region, c_region_size);
	PhoneNumberUtil::GetInstance()->GetSupportedTypesForRegion(region, &types);
	unmarshal_type_list(types, *c_types, *c_types_size);
}

extern "C" void c_phone_number_util_get_supported_types_for_non_geo_entity(
	int calling_code,
	phone_number_type **c_types,
	size_t *c_types_size
)
{
	std::set<PhoneNumberUtil::PhoneNumberType> types;
	PhoneNumberUtil::GetInstance()->GetSupportedTypesForNonGeoEntity(calling_code, &types);
	unmarshal_type_list(types, *c_types, *c_types_size);
}

extern "C" int c_phone_number_util_is_alpha_number(
	char const *c_number,
	size_t c_number_size
)
{
	std::string const number(c_number, c_number_size);
	return PhoneNumberUtil::GetInstance()->IsAlphaNumber(number);
}

extern "C" void c_phone_number_util_convert_alpha_characters_in_number(
	char const *c_input,
	size_t c_input_size,
	c_string *c_output
)
{
	std::string number(c_input, c_input_size);
	PhoneNumberUtil::GetInstance()->ConvertAlphaCharactersInNumber(&number);
	unmarshal_c_string(number, *c_output);
}

extern "C" void c_phone_number_util_normalize_digits_only(
	char const *c_input,
	size_t c_input_size,
	c_string *c_output
)
{
	std::string number(c_input, c_input_size);
	PhoneNumberUtil::GetInstance()->NormalizeDigitsOnly(&number);
	unmarshal_c_string(number, *c_output);
}

extern "C" void c_phone_number_util_normalize_dialable_chars_only(
	char const *c_input,
	size_t c_input_size,
	c_string *c_output
)
{
	std::string number(c_input, c_input_size);
	PhoneNumberUtil::GetInstance()->NormalizeDiallableCharsOnly(&number);
	unmarshal_c_string(number, *c_output);
}

extern "C" void c_phone_number_util_get_national_significant_number(
	CxxPhoneNumber const *ptr,
	c_string *c_nsn
)
{
	auto number = reinterpret_cast<PhoneNumber const *>(ptr);
	std::string nsn;
	PhoneNumberUtil::GetInstance()->GetNationalSignificantNumber(*number, &nsn);
	unmarshal_c_string(nsn, *c_nsn);
}

extern "C" void c_phone_number_util_get_country_mobile_token(
	int calling_code,
	c_string *c_token
)
{
	std::string token;
	PhoneNumberUtil::GetInstance()->GetCountryMobileToken(calling_code, &token);
	unmarshal_c_string(token, *c_token);
}

extern "C" void c_phone_number_util_format(
	CxxPhoneNumber const *ptr,
	phone_number_format format,
	c_string *c_string
)
{
	auto number = reinterpret_cast<PhoneNumber const *>(ptr);
	std::string string;
	PhoneNumberUtil::GetInstance()->Format(*number, marshal_phone_number_format(format), &string);
	unmarshal_c_string(string, *c_string);
}

extern "C" void c_phone_number_util_format_national_number_with_carrier_code(
	CxxPhoneNumber const *ptr,
	char const *c_carrier_code,
	size_t c_carrier_code_size,
	c_string *c_string
)
{
	auto number = reinterpret_cast<PhoneNumber const *>(ptr);
	std::string const carrier_code(c_carrier_code, c_carrier_code_size);
	std::string string;
	PhoneNumberUtil::GetInstance()->FormatNationalNumberWithCarrierCode(*number, carrier_code, &string);
	unmarshal_c_string(string, *c_string);
}

extern "C" void c_phone_number_util_format_national_number_with_preferred_carrier_code(
	CxxPhoneNumber const *ptr,
	char const *c_carrier_code,
	size_t c_carrier_code_size,
	c_string *c_string
)
{
	auto number = reinterpret_cast<PhoneNumber const *>(ptr);
	std::string const carrier_code(c_carrier_code, c_carrier_code_size);
	std::string string;
	PhoneNumberUtil::GetInstance()->FormatNationalNumberWithPreferredCarrierCode(*number, carrier_code, &string);
	unmarshal_c_string(string, *c_string);
}

extern "C" void c_phone_number_util_format_number_for_mobile_dialing(
	CxxPhoneNumber const *ptr,
	char const *c_region,
	size_t c_region_size,
	int with_formatting,
	c_string *c_string
)
{
	auto number = reinterpret_cast<PhoneNumber const *>(ptr);
	std::string const region(c_region, c_region_size);
	std::string string;
	PhoneNumberUtil::GetInstance()->FormatNumberForMobileDialing(*number, region, with_formatting, &string);
	unmarshal_c_string(string, *c_string);
}

extern "C" void c_phone_number_util_format_out_of_country_calling_number(
	CxxPhoneNumber const *ptr,
	char const *c_region,
	size_t c_region_size,
	c_string *c_string
)
{
	auto number = reinterpret_cast<PhoneNumber const *>(ptr);
	std::string const region(c_region, c_region_size);
	std::string string;
	PhoneNumberUtil::GetInstance()->FormatOutOfCountryCallingNumber(*number, region, &string);
	unmarshal_c_string(string, *c_string);
}

extern "C" void c_phone_number_util_format_in_original_format(
	CxxPhoneNumber const *ptr,
	char const *c_region,
	size_t c_region_size,
	c_string *c_string
)
{
	auto number = reinterpret_cast<PhoneNumber const *>(ptr);
	std::string const region(c_region, c_region_size);
	std::string string;
	PhoneNumberUtil::GetInstance()->FormatInOriginalFormat(*number, region, &string);
	unmarshal_c_string(string, *c_string);
}

extern "C" void c_phone_number_util_format_out_of_country_keeping_alpha_chars(
	CxxPhoneNumber const *ptr,
	char const *c_region,
	size_t c_region_size,
	c_string *c_string
)
{
	auto number = reinterpret_cast<PhoneNumber const *>(ptr);
	std::string const region(c_region, c_region_size);
	std::string string;
	PhoneNumberUtil::GetInstance()->FormatOutOfCountryKeepingAlphaChars(*number, region, &string);
	unmarshal_c_string(string, *c_string);
}

extern "C" CxxPhoneNumber *c_phone_number_util_truncate_too_long_number(CxxPhoneNumber const *ptr)
{
	auto input = reinterpret_cast<PhoneNumber const *>(ptr);
	auto *output = new PhoneNumber(*input);
	if (PhoneNumberUtil::GetInstance()->TruncateTooLongNumber(output))
	{
		return reinterpret_cast<CxxPhoneNumber *>(output);
	}
	else
	{
		delete output;
		return nullptr;
	}
}

extern "C" phone_number_type c_phone_number_util_get_number_type(CxxPhoneNumber const *ptr)
{
	auto number = reinterpret_cast<PhoneNumber const *>(ptr);
	return unmarshal_phone_number_type(
		PhoneNumberUtil::GetInstance()->GetNumberType(*number)
	);
}

extern "C" int c_phone_number_util_is_valid_number(CxxPhoneNumber const *ptr)
{
	auto number = reinterpret_cast<PhoneNumber const *>(ptr);
	return PhoneNumberUtil::GetInstance()->IsValidNumber(*number);
}

extern "C" int c_phone_number_util_is_valid_number_for_region(
	CxxPhoneNumber const *ptr,
	char const *c_region,
	size_t c_region_size
)
{
	auto number = reinterpret_cast<PhoneNumber const *>(ptr);
	std::string const region(c_region, c_region_size);
	return PhoneNumberUtil::GetInstance()->IsValidNumberForRegion(*number, region);
}

extern "C" void c_phone_number_util_get_region_code_for_number(
	CxxPhoneNumber const *ptr,
	c_string *c_region
)
{
	auto number = reinterpret_cast<PhoneNumber const *>(ptr);
	std::string region;
	PhoneNumberUtil::GetInstance()->GetRegionCodeForNumber(*number, &region);
	unmarshal_c_string(region, *c_region);
}

extern "C" int c_phone_number_util_get_country_code_for_region(
	char const *c_region,
	size_t c_region_size
)
{
	std::string const region(c_region, c_region_size);
	return PhoneNumberUtil::GetInstance()->GetCountryCodeForRegion(region);
}

extern "C" void c_phone_number_util_get_region_code_for_country_code(
	int country_code,
	c_string *c_region
)
{
	std::string region;
	PhoneNumberUtil::GetInstance()->GetRegionCodeForCountryCode(country_code, &region);
	unmarshal_c_string(region, *c_region);
}

extern "C" void c_phone_number_util_get_region_codes_for_country_calling_code(
	int country_code,
	c_string **c_regions,
	size_t *c_regions_size
)
{
	std::list<std::string> regions;
	PhoneNumberUtil::GetInstance()->GetRegionCodesForCountryCallingCode(country_code, &regions);
	unmarshal_string_list(regions, *c_regions, *c_regions_size);
}

extern "C" int c_phone_number_util_is_nanpa_country(
	char const *c_region,
	size_t c_region_size
)
{
	std::string const region(c_region, c_region_size);
	return PhoneNumberUtil::GetInstance()->IsNANPACountry(region);
}

extern "C" void c_phone_number_util_get_ndd_prefix_for_region(
	char const *c_region,
	size_t c_region_size,
	int strip_non_digits,
	c_string *c_prefix
)
{
	std::string const region(c_region, c_region_size);
	std::string prefix;
	PhoneNumberUtil::GetInstance()->GetNddPrefixForRegion(region, strip_non_digits, &prefix);
	unmarshal_c_string(prefix, *c_prefix);
}

extern "C" validation_result c_phone_number_util_is_possible_number_for_type_with_reason(
	CxxPhoneNumber const *ptr,
	phone_number_type ntype
)
{
	auto number = reinterpret_cast<PhoneNumber const *>(ptr);
	return unmarshal_validation_result(
		PhoneNumberUtil::GetInstance()->IsPossibleNumberForTypeWithReason(
			*number,
			marshal_phone_number_type(ntype)
		)
	);
}

extern "C" int c_phone_number_util_can_be_internationally_dialed(CxxPhoneNumber const *ptr)
{
	auto number = reinterpret_cast<PhoneNumber const *>(ptr);
	return PhoneNumberUtil::GetInstance()->CanBeInternationallyDialled(*number);
}

extern "C" int c_phone_number_util_is_number_geographical_1(CxxPhoneNumber const *ptr)
{
	auto number = reinterpret_cast<PhoneNumber const *>(ptr);
	return PhoneNumberUtil::GetInstance()->IsNumberGeographical(*number);
}

extern "C" int c_phone_number_util_is_number_geographical_2(
	phone_number_type ntype,
	int calling_code
)
{
	return PhoneNumberUtil::GetInstance()->IsNumberGeographical(
		marshal_phone_number_type(ntype),
		calling_code
	);
}

extern "C" error_type c_phone_number_util_parse(
	char const *c_string,
	size_t c_string_size,
	char const *c_region,
	size_t c_region_size,
	CxxPhoneNumber **number
)
{
	std::string const string(c_string, c_string_size);
	std::string const region(c_region, c_region_size);
	*number = reinterpret_cast<CxxPhoneNumber *>(new PhoneNumber());
	return unmarshal_error_type(
		PhoneNumberUtil::GetInstance()->Parse(string, region, reinterpret_cast<PhoneNumber *>(*number))
	);
}

extern "C" error_type c_phone_number_util_parse_and_keep_raw_input(
	char const *c_string,
	size_t c_string_size,
	char const *c_region,
	size_t c_region_size,
	CxxPhoneNumber **number
)
{
	std::string const string(c_string, c_string_size);
	std::string const region(c_region, c_region_size);
	*number = reinterpret_cast<CxxPhoneNumber *>(new PhoneNumber());
	return unmarshal_error_type(
		PhoneNumberUtil::GetInstance()->ParseAndKeepRawInput(string, region, reinterpret_cast<PhoneNumber *>(*number))
	);
}

extern "C" match_type c_phone_number_util_is_number_match(
	CxxPhoneNumber const *ptr1,
	CxxPhoneNumber const *ptr2
)
{
	auto number1 = reinterpret_cast<PhoneNumber const *>(ptr1);
	auto number2 = reinterpret_cast<PhoneNumber const *>(ptr2);
	return unmarshal_match_type(
		PhoneNumberUtil::GetInstance()->IsNumberMatch(*number1, *number2)
	);
}

extern "C" match_type c_phone_number_util_is_number_match_with_two_strings(
	char const *c_number1,
	size_t c_number1_size,
	char const *c_number2,
	size_t c_number2_size
)
{
	std::string const number1(c_number1, c_number1_size);
	std::string const number2(c_number2, c_number2_size);
	return unmarshal_match_type(
		PhoneNumberUtil::GetInstance()->IsNumberMatchWithTwoStrings(number1, number2)
	);
}

extern "C" match_type c_phone_number_util_is_number_match_with_one_string(
	CxxPhoneNumber const *ptr1,
	char const *c_number2,
	size_t c_number2_size
)
{
	auto number1 = reinterpret_cast<PhoneNumber const *>(ptr1);
	std::string const number2(c_number2, c_number2_size);
	return unmarshal_match_type(
		PhoneNumberUtil::GetInstance()->IsNumberMatchWithOneString(*number1, number2)
	);
}
