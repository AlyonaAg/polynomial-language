%{

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "Struct.h"

#pragma warning(disable : 4996)

FILE* yyin;
extern int yylineno;
void yyerror();

extern int yylex();

long long int res_mas[100];
LISTVAR list[50];
size_t count_var = 0;
size_t number_line = 0;
size_t end_flag = 0;

long long int* power_poly(long long int poly1[100], int power)
{
	if (!power)
	{
		int flag = 0;
		for (int i = 0; i < 100; i++)
		{
			if (poly1[i])
				flag = 1;
			res_mas[i] = 0;
		}
		if (!flag)
			yyerror("uncertainty arose (0^0)");
		res_mas[0] = 1;
	}
	else
	{
		long long int temp_mas[100];

		for (int i = 0; i < 100; i++)
		{
			temp_mas[i] = 0;
			res_mas[i] = poly1[i];
		}

		for (int k = 0; k < power - 1; k++)
		{
			for (int i = 0; i < 100; i++)
				for (int j = 0; j < 100; j++)
					if (poly1[i] && res_mas[j])
					{
						if (i + j >= 100)
							yyerror("too great a value of the degree.");
						temp_mas[i + j] += poly1[i] * res_mas[j];
					}

			for (int i = 0; i < 100; i++)
			{
				res_mas[i] = temp_mas[i];
				temp_mas[i] = 0;
			}
		}

	}
	return res_mas;
}

void print_poly(long long int poly1[100], char var)
{
	int flag = 0;
	for (int i = 99; i >= 0; i--)
	{
		if (poly1[i])
		{
			if (poly1[i] > 0 && flag)
				printf("+");

			if ((poly1[i] != 1 && poly1[i] != -1) || i == 0)
				printf("%lld", poly1[i]);
			if (poly1[i] == -1 && i != 0)
				printf("-");

			if (i > 1)
				printf("%c^%d", var, i);
			if (i == 1)
				printf("%c", var);

			flag = 1;
		}
	}
	if (!flag)
		printf("0");
	printf("\n");
}

int search_var(char name_var[12])
{
	for (int i = 0; i < count_var; i++)
		if (!strcmp(name_var, list[i].name_var))
			return i;
	return -1;
}

int empty_poly(long long int poly[100])
{
	for (int i = 0; i < 100; i++)
		if (poly[i])
			return 0;
	return 1;
}

void add_var(char name_var[12], long long int poly[100], char var)
{
	int result = search_var(name_var);
	if (result == -1)
	{
		if (count_var >= 50)
			yyerror("too many variables(maximum 50).");
		memcpy(list[count_var].name_var, name_var, sizeof(char) * strlen(name_var));
		memcpy(list[count_var].mas_poly, poly, sizeof(long long int) * 100);
		list[count_var].var = var;
		count_var++;
		//printf("NEW VAR: [%s]: ", list[count_var - 1].name_var);
		//print_poly(list[count_var - 1].mas_poly, list[count_var - 1].var);
	}
	else
	{
		memcpy(list[result].mas_poly, poly, sizeof(long long int) * 100);
		list[result].var = var;
		//printf("OLD VAR, BUT NEW POLY: [%s]: ", list[result].name_var);
		//print_poly(list[result].mas_poly, list[result].var);
	}
}

%}

%code requires {
	#include "Struct.h"
}

%start commands

%union{
	long long int ival;
	char cval;
	char sval[12];
	POLYVAL pval;
	MONOVAL mval;
}

%token MUL POWER OPENBRACKETS CLOSEBRACKETS DECLARATION EQUALLY PRINT SEMICOLON
%token <ival> NUMBER
%token <ival> SIGN
%token <cval> X
%token <sval> VAR

%type <ival> power
%type <ival> coefficient
%type <mval> variable
%type <mval> monomial
%type <pval> polynomial

%left SIGN
%left MUL
%left UMINUS

%%
commands: 
	command
	| 
	commands command
	;

power:
	POWER NUMBER
	{
		$$ = $2;
	}
	|
	POWER OPENBRACKETS polynomial CLOSEBRACKETS
	{
		for (int i = 1; i < 100; i++)
			if ($3.mas_poly[i])
				yyerror("degree of a variable cannot be a polynomial.");
		if ($3.mas_poly[0] < 0)
			yyerror("degree of a variable cannot be a negative number.");

		$$ = $3.mas_poly[0];
	}
	|
	POWER DECLARATION VAR
	{
		int result = search_var($3);
		if (result != -1)
		{
			for (int i = 1; i < 100; i++)
				if (list[result].mas_poly[i])
					yyerror("degree of a variable cannot be a polynomial.");
			if (list[result].mas_poly[0] < 0)
				yyerror("degree of a variable cannot be a negative number.");

			$$ = list[result].mas_poly[0];
		}
		else
			yyerror("undeclared variable.");
	}
	|
	power power
	{
		if (!$1 && !$2)
			yyerror("uncertainty arose (0^0).");
		long long int result = 1;
		for (int i = 0; i < $2; i++)
			result *= $1;
		$$ = result;
	}
	;

coefficient:
	NUMBER
	{
		$$ = $1;
	}
	|
	NUMBER power
	{
		if (!$1 && !$2)
			yyerror("uncertainty arose (0^0).");
		if (!$2)
			$$ = 1;
		else
		{
			long long int result = 1;
			for (int i = 0; i < $2; i++)
				result *= $1;
			$$ = result;
		}
	}
	|
	SIGN coefficient %prec UMINUS
	{
		$$ = -$2;
	}
	;

variable:
	X
	{
		$$.var = $1;
		$$.power = 1;
	}
	|
	X power
	{
		$$.var = $1;
		if ($2 >= 100)
			yyerror("too great a value of the degree.");
		$$.power = $2;
	}
	|
	VAR
	{
		yyerror("using polynomial name for variable name.");
	}
	|
	VAR power
	{
		yyerror("using polynomial name for variable name.");
	}
	;

monomial:
	coefficient
	{
		$$.power = 0;
		$$.coeff = $1;
		$$.var = '0';
	}
	|
	variable
	{
		$$.power = $1.power;
		$$.coeff = 1;
		$$.var = $1.var;
	}
	|
	coefficient variable
	{
		$$.power = $2.power;
		$$.coeff = $1;
		$$.var = $2.var;
	}
	;

polynomial:
	monomial
	{
		$$.var = $1.var;
		for (int i = 0; i < 100; i++)
			$$.mas_poly[i] = 0;
		$$.mas_poly[$1.power] = $1.coeff;
	}
	|
	DECLARATION VAR
	{
		int result = search_var($2);
		if (result != -1)
		{
			$$.var = list[result].var;
			memcpy($$.mas_poly, list[result].mas_poly, 100);
		}
		else
			yyerror("undeclared variable.");
	}
	|
	OPENBRACKETS polynomial CLOSEBRACKETS
	{	
		$$.var = $2.var;
		for (int i = 0; i < 100; i++)
			$$.mas_poly[i] = $2.mas_poly[i];
	}
	|
	polynomial SIGN polynomial
	{
		if ($1.var != $3.var && $1.var != '0' && $3.var != '0')
			yyerror("there are different variables in the polynomial.");
		if ($1.var == '0')
			$$.var = $3.var;
		else
			$$.var = $1.var;

		for (int i = 0; i < 100; i++)
			$$.mas_poly[i] = $1.mas_poly[i] + $2 * $3.mas_poly[i];

		if (empty_poly($$.mas_poly))
			$$.var = '0';
	}
	|
	polynomial MUL polynomial
	{
		if ($1.var != $3.var && $1.var != '0' && $3.var != '0')
			yyerror("there are different variables in the polynomial.");
		if ($1.var == '0')
			$$.var = $3.var;
		else
			$$.var = $1.var;

		for (int i = 0; i < 100; i++)
			$$.mas_poly[i] = 0;
		for (int i = 0; i < 100; i++)
			for (int j = 0; j < 100; j++)
				if ($1.mas_poly[i] && $3.mas_poly[j])
				{
					if (i + j >= 100)
						yyerror("too great a value of the degree.");
					$$.mas_poly[i + j] += $1.mas_poly[i] * $3.mas_poly[j];
				}

		if (empty_poly($$.mas_poly))
			$$.var = '0';
	}
	|
	polynomial power
	{
		$$.var = $1.var;
		memcpy($$.mas_poly, power_poly($1.mas_poly, $2), sizeof(long long int) * 100);
	}
	|
	SIGN polynomial %prec UMINUS
	{
		$$.var = $2.var;
		for (int i = 0; i < 100; i++)
			$$.mas_poly[i] = (-1) * $2.mas_poly[i];
	}
	;

command:
	DECLARATION VAR EQUALLY polynomial SEMICOLON
	{
		add_var($2, $4.mas_poly, $4.var);
	}
	| 
	PRINT OPENBRACKETS polynomial CLOSEBRACKETS SEMICOLON
	{
		print_poly($3.mas_poly, $3.var);
	}
	|
	VAR EQUALLY polynomial SEMICOLON
	{
		yyerror("missing '$'.");
	}
	;
%%

int parser_main(int argc, char* argv[])
{
	if (argc > 2)
	{
		printf("Error: too many parameters\n");
		return 1;
	}
	if (argc == 2)
	{
		yyin = fopen(argv[1], "rb");
		if (yyin == NULL)
		{
			printf("Error: file wasn't opened\n");
			return 1;
		}
		yyparse();
		fclose(yyin);
	}
	return 0;
}

void yyerror(const char* error_string)
{
	printf("Error: %s [%d line]\n", error_string, number_line+1);
	exit(1);
}