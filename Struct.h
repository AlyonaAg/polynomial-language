#pragma once
typedef struct polyval {
	char var;
	long long int mas_poly[100];
} POLYVAL;

typedef struct monoval {
	char var;
	int power;
	long long int coeff;
} MONOVAL;

typedef struct ListVar
{
	char name_var[12];
	char var;
	long long int mas_poly[100];
}LISTVAR;