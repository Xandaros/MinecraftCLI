#ifndef __CRYPT_H__
#define __CRYPT_H__

#include <mcrypt.h>

MCRYPT *newCipher(char *key, char *iv);
void closeCipher(MCRYPT *td);
void encrypt(MCRYPT *td, char *plain, int len);
void decrypt(MCRYPT *td, char *cipher, int len);

#endif //__CRYPT_H__
