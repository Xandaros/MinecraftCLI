#include "crypt.h"

#include <mcrypt.h>
#include <stdlib.h>
#include <string.h>

MCRYPT *newCipher(char *key, char *iv)
{
	MCRYPT td = mcrypt_module_open("rijndael-128", NULL, "cfb", NULL);
	int iv_size = mcrypt_enc_get_iv_size(td);
	char *IV = malloc(iv_size);
	memcpy(IV, iv, iv_size);
	mcrypt_generic_init(td, key, 16, iv);

	MCRYPT *td_ptr = malloc(sizeof(td));
	memcpy(td_ptr, &td, sizeof(td));
	return td_ptr;
}

void closeCipher(MCRYPT *td)
{
	mcrypt_generic_deinit(*td);
	mcrypt_module_close(*td);
	free(td);
}

void decrypt(MCRYPT *td, char *cipher, int len)
{
	mdecrypt_generic(*td, cipher, len);
}

void encrypt(MCRYPT *td, char *plain, int len)
{
	mcrypt_generic(*td, plain, len);
}
