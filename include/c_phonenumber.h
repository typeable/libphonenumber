#ifndef c_phonenumber_h
#define c_phonenumber_h

#include <stdint.h>
#include <stddef.h>

// Copied from PhoneNumber::CountryCodeSource.
// is_same_enum verifies that these values didn't change
enum country_code_source
{
	UNSPECIFIED = 0,
	FROM_NUMBER_WITH_PLUS_SIGN = 1,
	FROM_NUMBER_WITH_IDD = 5,
	FROM_NUMBER_WITHOUT_PLUS_SIGN = 10,
	FROM_DEFAULT_COUNTRY = 20,
};

// PhoneNumber is not a POD (not trivially constructible, not trivially
// moveable), and all of its data is accessed through methods. To access its
// data from haskell, we convert a PhoneNumber into an auxiliary struct all at
// once. The struct is a POD and can be parsed by haskell.
typedef struct
{
	// Strings are allocated by malloc and it is haskell's responsibility to
	// free them. nullptr indicates the string is absent, in which case its
	// corresponding size is uninitialized.
	char *extension;
	size_t extension_size;
	char *raw_input;
	size_t raw_input_size;
	char *preferred_domestic_carrier_code;
	size_t preferred_domestic_carrier_code_size;
	uint64_t national_number;
	int32_t country_code;
	// Uninitialized if !has_number_of_leading_zeros
	int32_t number_of_leading_zeros;
	// Unininitialized if !has_country_code_source
	enum country_code_source country_code_source;
	// Unininitialized if !has_italian_leading_zero
	char italian_leading_zero;
	char has_italian_leading_zero;
	char has_country_code_source;
	char has_number_of_leading_zeros;
} phone_number;

struct CxxPhoneNumber;

// The returned object is allocated by `new` and managed by C++ RAII, and it is
// haskell's responsibility to call its finalizer (c_phone_number_free).
struct CxxPhoneNumber *c_phone_number_marshal(phone_number const *);

// phone_number * points to an area which will be filled with the data of the
// given PhoneNumber.
void c_phone_number_unmarshal(struct CxxPhoneNumber const *, phone_number *);

void c_phone_number_free(struct CxxPhoneNumber *);

#endif
