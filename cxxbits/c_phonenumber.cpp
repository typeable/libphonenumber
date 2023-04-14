#include <string>
#include <cstdlib>
#include <phonenumbers/phonenumberutil.h>

#include "common.h"
extern "C" {
#include "c_phonenumber.h"
}

using i18n::phonenumbers::PhoneNumber;

static PhoneNumber::CountryCodeSource marshal_country_code_source(country_code_source value)
{
	static_assert(is_same_enum<
			enum_values<country_code_source,
				UNSPECIFIED,
				FROM_NUMBER_WITH_PLUS_SIGN,
				FROM_NUMBER_WITH_IDD,
				FROM_NUMBER_WITHOUT_PLUS_SIGN,
				FROM_DEFAULT_COUNTRY
			>,
			enum_values<PhoneNumber::CountryCodeSource,
				PhoneNumber::UNSPECIFIED,
				PhoneNumber::FROM_NUMBER_WITH_PLUS_SIGN,
				PhoneNumber::FROM_NUMBER_WITH_IDD,
				PhoneNumber::FROM_NUMBER_WITHOUT_PLUS_SIGN,
				PhoneNumber::FROM_DEFAULT_COUNTRY
			>
		>::value,
		"CountryCodeSource enum changed"
	);
	return static_cast<PhoneNumber::CountryCodeSource>(value);
}

static country_code_source unmarshal_country_code_source(PhoneNumber::CountryCodeSource value)
{
	static_assert(is_same_enum<
			enum_values<country_code_source,
				UNSPECIFIED,
				FROM_NUMBER_WITH_PLUS_SIGN,
				FROM_NUMBER_WITH_IDD,
				FROM_NUMBER_WITHOUT_PLUS_SIGN,
				FROM_DEFAULT_COUNTRY
			>,
			enum_values<PhoneNumber::CountryCodeSource,
				PhoneNumber::UNSPECIFIED,
				PhoneNumber::FROM_NUMBER_WITH_PLUS_SIGN,
				PhoneNumber::FROM_NUMBER_WITH_IDD,
				PhoneNumber::FROM_NUMBER_WITHOUT_PLUS_SIGN,
				PhoneNumber::FROM_DEFAULT_COUNTRY
			>
		>::value,
		"CountryCodeSource enum changed"
	);
	return static_cast<country_code_source>(value);
}

extern "C" CxxPhoneNumber *c_phone_number_marshal(phone_number const *c_number)
{
	PhoneNumber *number = new PhoneNumber();
	if (c_number->extension != nullptr)
		number->set_extension(c_number->extension, c_number->extension_size);
	if (c_number->raw_input != nullptr)
		number->set_raw_input(c_number->raw_input, c_number->raw_input_size);
	if (c_number->preferred_domestic_carrier_code != nullptr)
		number->set_preferred_domestic_carrier_code(c_number->preferred_domestic_carrier_code, c_number->preferred_domestic_carrier_code_size);
	number->set_national_number(c_number->national_number);
	number->set_country_code(c_number->country_code);
	if (c_number->has_italian_leading_zero)
		number->set_italian_leading_zero(c_number->italian_leading_zero);
	if (c_number->has_country_code_source)
		number->set_country_code_source(marshal_country_code_source(c_number->country_code_source));
	if (c_number->has_number_of_leading_zeros)
		number->set_number_of_leading_zeros(c_number->number_of_leading_zeros);
	return reinterpret_cast<CxxPhoneNumber *>(number);
}

extern "C" void c_phone_number_unmarshal(CxxPhoneNumber const *ptr, phone_number *c_number)
{
	auto number = reinterpret_cast<PhoneNumber const *>(ptr);
	if (number->has_extension())
		unmarshal_string_size(number->extension(), c_number->extension, c_number->extension_size);
	else
		c_number->extension = nullptr;
	if (number->has_raw_input())
		unmarshal_string_size(number->raw_input(), c_number->raw_input, c_number->raw_input_size);
	else
		c_number->raw_input = nullptr;
	if (number->has_preferred_domestic_carrier_code())
		unmarshal_string_size(number->preferred_domestic_carrier_code(), c_number->preferred_domestic_carrier_code, c_number->preferred_domestic_carrier_code_size);
	else
		c_number->preferred_domestic_carrier_code = nullptr;
	c_number->national_number = number->national_number();
	c_number->country_code = number->country_code();
	if (c_number->has_italian_leading_zero = number->has_italian_leading_zero())
		c_number->italian_leading_zero = number->italian_leading_zero();
	if (c_number->has_country_code_source = number->has_country_code_source())
		c_number->country_code_source = unmarshal_country_code_source(number->country_code_source());
	if (c_number->has_number_of_leading_zeros = number->has_number_of_leading_zeros())
		c_number->number_of_leading_zeros = number->number_of_leading_zeros();
}

extern "C" void c_phone_number_free(CxxPhoneNumber *ptr)
{
	auto number = reinterpret_cast<PhoneNumber *>(ptr);
	delete number;
}
