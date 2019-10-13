/*	Definition section */
%{
#include <stdio.h>
#include <string.h>


extern int yylineno;
extern int yylex();
extern char* yytext;   // Get current token from lex
extern char buf[256];  // Get current code line from lex
extern char syntaxErrorLine[256];
extern int dumpOrNot;
extern int dumpSementic;
extern int dumpSyntax;
extern int elseDump;
extern void keepScan();

void yyerror(char *s);
void sementicError(char* s, char* variable);

void genCode(char *code);

typedef struct{
    int Index;
    char* Name;
    char* Kind;
    char* Type;
    int Scope;
    char* Attribute;
    int functionForward;
    int stackID;
    int Ivalue;
    float Fvalue;
    char* Svalue;
}symbolTable;
symbolTable table[300];

int shutDown=0;
int currentScope=0;
int currentSymbolNumber=0;
int symbolTablesIndex[300];
char* sementicErrorList[4];
FILE *file;

int localVariable[100];
int localVariable_flag=0;
int globalDecl_flag=0;
int labelWhile_flag=0;
int labelIf[100];
int labelIf_flag=0;
int labelThen[100];
int labelThen_flag=0;
int  errorr_flag=0;
int loadZero_flag=0;

/* Symbol table function - you can add new function if needed. */
void create_symbol(int x, int entry);
void insert_symbol(int Index, char* Name,char* Kind, char* Type, int Scope, char* Attribute, int functionForward, int stackID);
int lookup_symbol(char* Name, int Scope, int Funforward, char* Attribute, char* Type);
void dump_symbol(int currentScope);

%}

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    double f_val;
    char* string;
}

/* Token without return */
%token ADD SUB MUL DIV MOD INC DEC 
%token MT LT MTE LTE EQ NE ASGN ADDASGN SUBASGN MULASGN DIVASGN MODASGN
%token AND OR NOT
%token LB RB LCB RCB LSB RSB
%token COMMA QUOTA
%token PRINT IF ELSE FOR WHILE
%token TRUE FALSE
%token RETURN CONTINUE BREAK

/* Token with return, which need to sepcify type */
%token <i_val> I_CONST
%token <f_val> F_CONST
%token <string> STRING
%token <string> TSTRING
%token <string> INT
%token <string> FLOAT
%token <string> VOID
%token <string> BOOL
%token <string> ID
%token <string> SEMICOLON
/* Nonterminal with return, which need to sepcify type */
%type <string> type

%type <string> parameter
%type <string> parameters
%type <string> parameter_list

%type <string> expression_stat
%type <string> initializer id_or_init
%type <string> expression ret
%type <string> arithmetic_stat arithmetic_temp1 arithmetic_temp2
%type <string> logic_stat logic_temp1 logic_temp2 logic_temp3 logics compare_stat
%type <string> inc_op add_op mul_op comp_op asgn_op
%type <string> function_call function_call_parameter function_call_parameters function_call_parameter_list

/* Yacc will start at this nonterminal */
%start program

/* Grammar section */
%%
program
    : stats
;

stats
    : stat stats                                
    | 
;

stat
    : declaration
    | compound_stat
    | expression_stat
    | if_stat
    | while_stat
    | print_func
;

declaration
    : type ID SEMICOLON     {
        if(currentScope==0)
        {
            genCode(".field public static ");
            genCode($2);
            if(strcmp($1,"int")==0)
            {
                genCode(" I");
            }
            else if(strcmp($1,"float")==0)
            {
                genCode(" F");
            }
            else if(strcmp($1,"string")==0)
            {
                genCode(" Ljava/lang/String;");
            }
            else if(strcmp($1,"bool")==0)
            {
                genCode(" Z");
            }
            else if(strcmp($1,"void")==0)
            {
                genCode(" V");
            }
            genCode("\n");
            insert_symbol(symbolTablesIndex[currentScope], $2, "variable", $1, currentScope, "", 0,-1);
        }
        else
        {
            char *temp=(char*)malloc(10);
            sprintf(temp,"%d", localVariable_flag);
            if(strcmp($1,"int")==0)
            {
                genCode("\tldc 0\n");
                genCode("\tistore ");
                genCode(temp);
                genCode("\n");
            }
            else if(strcmp($1,"float")==0)
            {
                genCode("\tldc 0.0\n");
                genCode("\tfstore ");
                genCode(temp);
                genCode("\n");
            }
            else if(strcmp($1,"string")==0)
            {
                genCode("\tldc \"\"\n");
                genCode("\tastore ");
                genCode(temp);
                genCode("\n");
            }
            else if(strcmp($1,"bool")==0)
            {
                genCode("\tldc 0\n");
                genCode("\tistore ");
                genCode(temp);
                genCode("\n");
            }
            insert_symbol(symbolTablesIndex[currentScope], $2, "variable", $1, currentScope, "", 0,localVariable_flag);
            localVariable[localVariable_flag]=currentSymbolNumber;
            localVariable_flag+=1;
        }
        symbolTablesIndex[currentScope]+=1;
    }
    | type ID ASGN {
        if(currentScope==0)
        {
             globalDecl_flag = 1;   
        }
    } expression SEMICOLON  {
        if(currentScope==0)
        {
            genCode(".field public static ");
            genCode($2);
            if(strcmp($1,"int")==0)
            {
                genCode(" I = ");
            }
            else if(strcmp($1,"float")==0)
            {
                genCode(" F = ");
            }
            else if(strcmp($1,"string")==0)
            {
                genCode(" Ljava/lang/String; =");
            }
            else if(strcmp($1,"bool")==0)
            {
                genCode(" Z = ");
            }
            else if(strcmp($1,"void")==0)
            {
                genCode(" V = ");
            }
            genCode($5);
            genCode("\n");
            globalDecl_flag = 0;
            insert_symbol(symbolTablesIndex[currentScope], $2, "variable", $1, currentScope, "", 0,-1);
 
        }
        else
        {
            char *temp=(char*)malloc(10);
            sprintf(temp,"%d", localVariable_flag);
            if(strcmp($1,"int")==0)
            {
                if(strcmp($5,"Tfloat")==0)
                {
                    genCode("\tf2i\n");
                }
                genCode("\tistore ");
                genCode(temp);
                genCode("\n");
            }
            else if(strcmp($1,"float")==0)
            {
                if(strcmp($5,"Tint")==0)
                {
                    genCode("\ti2f\n");
                }
                genCode("\tfstore ");
                genCode(temp);
                genCode("\n");
            }
            else if(strcmp($1,"string")==0)
            {
                genCode("\tastore ");
                genCode(temp);
                genCode("\n");
            }
            else if(strcmp($1,"bool")==0)
            {
                genCode("\tistore ");
                genCode(temp);
                genCode("\n");
            }
            insert_symbol(symbolTablesIndex[currentScope], $2, "variable", $1, currentScope, "", 0,localVariable_flag);
            localVariable[localVariable_flag]=currentSymbolNumber;
            localVariable_flag+=1;
        }
        //insert_symbol(symbolTablesIndex[currentScope], $2, "variable", $1, currentScope, "", 0, -1);
        symbolTablesIndex[currentScope]+=1;
    }
    | function_declaration             
;

initializer
    : I_CONST   {
        char* temp=(char*)malloc(30);
        sprintf(temp, "%d", yylval.i_val);
        if(globalDecl_flag==1)
            $$ = temp;
        else
        {
            genCode("\tldc ");
            genCode(temp);
            if(strcmp(temp,"0")==0)
            {
                loadZero_flag = 1;
            }
            genCode("\n");
            $$ = "Tint";
        }
    }
    | F_CONST   {
        char* temp=(char*)malloc(30);
        sprintf(temp, "%f", yylval.f_val);
        if(globalDecl_flag==1)
            $$ = temp;
        else
        {
            genCode("\tldc ");
            genCode(temp);
            if(strcmp(temp,"0.0")==0)
            {
                loadZero_flag = 1;
            }
            genCode("\n");
            $$ = "Tfloat";
        }
    }
    | STRING    {
        if(globalDecl_flag==1)
            $$ = $1;
        else
        {
            genCode("\tldc ");
            genCode($1);
            genCode("\n");
            $$ = "Tstring";
        }
    }
    | TRUE      {
        if(globalDecl_flag==1)
            $$ = "1";
        else
        {
            genCode("\tldc 1\n");
            $$ = "Tint";
        }
    }
    | FALSE     {
        if(globalDecl_flag==1)
            $$ = "0";
        else
        {
            genCode("\tldc 0\n");
            $$ = "Tint";
        }
    }
;

function_declaration
    : type ID LB parameter_list RB SEMICOLON    { 
        currentScope+=1;
        char* AttrTemp = (char*)malloc(100);
        char* pch;
        char *saveptrPch = NULL;
        strcpy(AttrTemp,"");
        pch = strtok_r($4,",",&saveptrPch);
        while (pch != NULL)
        {
            char* ParaTemp;
            char* saveptrPara;
            ParaTemp = strtok_r(pch," ",&saveptrPara);
            if(strlen(AttrTemp)==0)
            {
                strcat(AttrTemp, ParaTemp);  
            }
            else
                sprintf(AttrTemp, "%s, %s", AttrTemp, ParaTemp);
            pch = strtok_r(NULL, ",",&saveptrPch);
        }
        
        currentScope-=1;
        insert_symbol(symbolTablesIndex[currentScope], $2, "function", $1, currentScope, AttrTemp, 1, -1);
        symbolTablesIndex[currentScope]+=1;
    }
    | type ID LB parameter_list RB  {
        genCode(".method public static ");
        genCode($2);
        genCode("(");
        if(strcmp($2,"main")==0)
        {
            if(strcmp($4,"")!=0)
            {
                yyerror("main function have parameters");
            }
            else
            {
                genCode("[Ljava/lang/String;");
            }
        }
        
        currentScope+=1;
        char* AttrTemp = (char*)malloc(100);
        char* pch;
        char *saveptrPch = NULL;
        strcpy(AttrTemp,"");
        pch = strtok_r($4,",",&saveptrPch);
        while (pch != NULL)
        {
            char* ParaTemp;
            char* saveptrPara;
            ParaTemp = strtok_r(pch," ",&saveptrPara);
            if(strcmp($2,"main")!=0)
            {
                if(strcmp(ParaTemp,"int")==0)
                {
                    genCode("I");
                }
                else if(strcmp(ParaTemp,"float")==0)
                {
                    genCode("F");
                }
                else if(strcmp(ParaTemp,"bool")==0)
                {
                    genCode("Z");
                }
                else if(strcmp(ParaTemp,"void")==0)
                {
                    genCode("V");
                }
                else if(strcmp(ParaTemp,"string")==0)
                {
                    genCode("Ljava/lang/String;");
                }
            }
            if(strlen(AttrTemp)==0)
            {
                strcat(AttrTemp, ParaTemp);
                
            }
            else
                sprintf(AttrTemp, "%s, %s", AttrTemp, ParaTemp);
            /**/insert_symbol(symbolTablesIndex[currentScope], saveptrPara, "parameter", ParaTemp, currentScope, "", 0, localVariable_flag);
            symbolTablesIndex[currentScope]+=1;
            pch = strtok_r(NULL, ",",&saveptrPch);
        }
        
        currentScope-=1;
        insert_symbol(symbolTablesIndex[currentScope], $2, "function", $1, currentScope, AttrTemp, 0, -1);
        symbolTablesIndex[currentScope]+=1;
        currentScope+=1;

        genCode(")");
        if(strcmp($1,"int")==0)
        {
            genCode("I");
        }
        else if(strcmp($1,"float")==0)
        {
            genCode("F");
        }
        else if(strcmp($1,"bool")==0)
        {
            genCode("Z");
        }
        else if(strcmp($1,"void")==0)
        {
            genCode("V");
        }
        else if(strcmp($1,"string")==0)
        {
            genCode("[Ljava/lang/String;");
        }
        genCode("\n");
        genCode(".limit stack 50\n");
        genCode(".limit locals 50\n");
    }
    LCB stats ret RCB   {
        dumpOrNot=1;
        // dump_symbol(currentScope);
        // currentScope -= 1;

        if(strcmp($9,";")==0)
            genCode("\treturn\n");
        else if(strcmp($9,"")==0)
        {
            genCode("\treturn\n");
        }
        else
        {
            if(strcmp($1,"int")==0)
            {
                if(strcmp($9,"Tint")==0)
                {
                    genCode("\tireturn\n");
                }
                else if(strcmp($9,"Tfloat")==0)
                {
                    genCode("\tf2i\n");
                    genCode("\tireturn\n");
                }
                
            }
            else if(strcmp($1,"float")==0)
            {
                if(strcmp($9,"Tint")==0)
                {
                    genCode("\ti2f\n");
                    genCode("\tfreturn\n");
                }
                else if(strcmp($9,"Tfloat")==0)
                {
                    genCode("\tfreturn\n");
                }
            }
            else if(strcmp($1,"string")==0)
            {
                genCode("\tareturn\n");
            }
        }
        genCode(".end method\n");
    }
;

parameter_list
    : parameter parameters                  {
                                                if(strlen($2)>0){
                                                    sprintf($$, "%s,%s", strdup($1), strdup($2));
                                                }
                                                else{
                                                    $$=$1;
                                                }
                                            }
;

parameters
    : COMMA parameter_list                  {$$=$2;}
    |                                       {$$="";}
;

/**/
parameter
    : type ID                               { sprintf($$, "%s %s", strdup($1), strdup($2)); }
    | type ID ASGN initializer              { sprintf($$, "%s %s", strdup($1), strdup($2)); }
    |                                       {$$="";}
;

ret
    : RETURN expression_stat    {
        $$ = $2;
    }
    |   {
        $$ = "";
    }
;

compound_stat
	: LCB {currentScope+=1;} stats RCB { dumpOrNot=1; }
;

expression_stat
    : expression SEMICOLON      {$$ = $1;}
    | SEMICOLON                 {$$ = ";";}
;

if_stat
    : IF LB expression {
        labelIf[labelIf_flag]+=1;
        char* ifTempNest = (char*)malloc(10);
        char* ifTempNum = (char*)malloc(10);
        sprintf(ifTempNest, "%d", labelIf_flag);
        sprintf(ifTempNum, "%d", labelIf[labelIf_flag]);
        if(strcmp($3,"Tint>")==0)
        {
            genCode("\tisub\n");
            genCode("\tifgt LABEL_IF_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_IF_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($3,"Tint<")==0)
        {
            genCode("\tisub\n");
            genCode("\tiflt LABEL_IF_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_IF_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($3,"Tint>=")==0)
        {
            genCode("\tisub\n");
            genCode("\tifge LABEL_IF_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_IF_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($3,"Tint<=")==0)
        {
            genCode("\tisub\n");
            genCode("\tifle LABEL_IF_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_IF_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($3,"Tint==")==0)
        {
            genCode("\tisub\n");
            genCode("\tifeq LABEL_IF_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_IF_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($3,"Tint!=")==0)
        {
            genCode("\tisub\n");
            genCode("\tifne LABEL_IF_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_IF_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($3,"Tfloat>")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tifgt LABEL_IF_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_IF_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($3,"Tfloat<")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tiflt LABEL_IF_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_IF_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($3,"Tfloat>=")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tifge LABEL_IF_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_IF_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($3,"Tfloat<=")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tifle LABEL_IF_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_IF_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($3,"Tfloat==")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tifeq LABEL_IF_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_IF_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($3,"Tfloat!=")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tifne LABEL_IF_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_IF_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        genCode("LABEL_IF_TRUE_");
        genCode(ifTempNest);
        genCode("_");
        genCode(ifTempNum);
        genCode(":\n");
        labelIf_flag+=1;
        labelIf[labelIf_flag]=0;
    } RB compound_stat {
        elseDump=1;

        labelIf[labelIf_flag]=0;
        labelIf_flag-=1;

        char* ifTempNest = (char*)malloc(10);
        char* ifTempNum = (char*)malloc(10);
        sprintf(ifTempNest, "%d", labelIf_flag);
        sprintf(ifTempNum, "%d", labelIf[labelIf_flag]);

        genCode("\tgoto LABEL_IF_END_");
        genCode(ifTempNest);
        genCode("_");
        genCode(ifTempNum);
        genCode("\n");

        genCode("LABEL_IF_FALSE_");
        genCode(ifTempNest);
        genCode("_");
        genCode(ifTempNum);
        genCode(":\n");

    } else_if_stat else_stat {
        char* ifTempNest = (char*)malloc(10);
        char* ifTempNum = (char*)malloc(10);
        sprintf(ifTempNest, "%d", labelIf_flag);
        sprintf(ifTempNum, "%d", labelIf[labelIf_flag]);
        genCode("LABEL_IF_END_");
        genCode(ifTempNest);
        genCode("_");
        genCode(ifTempNum);
        genCode(":\n");
    }
;
else_if_stat
    : else_if_stat ELSE IF LB expression {
        labelThen[labelIf_flag]+=1;
        char* ifTempNest = (char*)malloc(10);
        char* ifTempNum = (char*)malloc(10);
        char* thenTemp = (char*)malloc(10);
        sprintf(ifTempNest, "%d", labelIf_flag);
        sprintf(ifTempNum, "%d", labelIf[labelIf_flag]);
        sprintf(thenTemp, "%d", labelThen[labelIf_flag]);
        if(strcmp($5,"Tint>")==0)
        {
            genCode("\tisub\n");
            genCode("\tifgt LABEL_THEN_");
            genCode(thenTemp);
            genCode("_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_THEN_");
            genCode(thenTemp);
            genCode("_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($5,"Tint<")==0)
        {
            genCode("\tisub\n");
            genCode("\tiflt LABEL_THEN_");
            genCode(thenTemp);
            genCode("_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_THEN_");
            genCode(thenTemp);
            genCode("_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($5,"Tint>=")==0)
        {
            genCode("\tisub\n");
            genCode("\tifge LABEL_THEN_");
            genCode(thenTemp);
            genCode("_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_THEN_");
            genCode(thenTemp);
            genCode("_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($5,"Tint<=")==0)
        {
            genCode("\tisub\n");
            genCode("\tifle LABEL_THEN_");
            genCode(thenTemp);
            genCode("_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_THEN_");
            genCode(thenTemp);
            genCode("_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($5,"Tint==")==0)
        {
            genCode("\tisub\n");
            genCode("\tifeq LABEL_THEN_");
            genCode(thenTemp);
            genCode("_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_THEN_");
            genCode(thenTemp);
            genCode("_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($5,"Tint!=")==0)
        {
            genCode("\tisub\n");
            genCode("\tifne LABEL_THEN_");
            genCode(thenTemp);
            genCode("_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_THEN_");
            genCode(thenTemp);
            genCode("_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($5,"Tfloat>")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tifgt LABEL_THEN_");
            genCode(thenTemp);
            genCode("_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_THEN_");
            genCode(thenTemp);
            genCode("_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($5,"Tfloat<")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tiflt LABEL_THEN_");
            genCode(thenTemp);
            genCode("_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_THEN_");
            genCode(thenTemp);
            genCode("_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($5,"Tfloat>=")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tifge LABEL_THEN_");
            genCode(thenTemp);
            genCode("_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_THEN_");
            genCode(thenTemp);
            genCode("_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($5,"Tfloat<=")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tifle LABEL_THEN_");
            genCode(thenTemp);
            genCode("_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_THEN_");
            genCode(thenTemp);
            genCode("_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($5,"Tfloat==")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tifeq LABEL_THEN_");
            genCode(thenTemp);
            genCode("_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_THEN_");
            genCode(thenTemp);
            genCode("_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        else if(strcmp($5,"Tfloat!=")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tifne LABEL_THEN_");
            genCode(thenTemp);
            genCode("_TRUE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
            genCode("\tgoto LABEL_THEN_");
            genCode(thenTemp);
            genCode("_FALSE_");
            genCode(ifTempNest);
            genCode("_");
            genCode(ifTempNum);
            genCode("\n");
        }
        genCode("LABEL_THEN_");
        genCode(thenTemp);
        genCode("_TRUE_");
        genCode(ifTempNest);
        genCode("_");
        genCode(ifTempNum);
        genCode(":\n");
        labelIf_flag+=1;
        labelIf[labelIf_flag]=0;
        labelThen[labelIf_flag]=0;
    } RB compound_stat {
        elseDump=1;

        labelIf[labelIf_flag]=0;
        labelThen[labelIf_flag]=0;
        labelIf_flag-=1;

        char* ifTempNest = (char*)malloc(10);
        char* ifTempNum = (char*)malloc(10);
        char* thenTemp = (char*)malloc(10);
        sprintf(ifTempNest, "%d", labelIf_flag);
        sprintf(ifTempNum, "%d", labelIf[labelIf_flag]);
        sprintf(thenTemp, "%d", labelThen[labelIf_flag]);

        genCode("\tgoto LABEL_IF_END_");
        genCode(ifTempNest);
        genCode("_");
        genCode(ifTempNum);
        genCode("\n");

        genCode("LABEL_THEN_");
        genCode(thenTemp);
        genCode("_FALSE_");
        genCode(ifTempNest);
        genCode("_");
        genCode(ifTempNum);
        genCode(":\n");
    }
    |   
;
else_stat
    : ELSE {
        labelIf_flag+=1;
        labelIf[labelIf_flag]=0;
        labelThen[labelIf_flag]=0;
    } compound_stat {
        labelIf[labelIf_flag]=0;
        labelThen[labelIf_flag]=0;
        labelIf_flag-=1;
    }
    |
;


while_stat
    : WHILE {
        genCode("LABEL_BEGIN_");
        char* whileTemp = (char*)malloc(10);
        sprintf(whileTemp, "%d", labelWhile_flag);
        genCode(whileTemp);
        genCode(":\n");
    } LB expression RB {
        char* whileTemp = (char*)malloc(10);
        sprintf(whileTemp, "%d", labelWhile_flag);
        if(strcmp($4,"Tint>")==0)
        {
            genCode("\tisub\n");
            genCode("\tifgt LABEL_TRUE_");
            genCode(whileTemp);
            genCode("\n");
            genCode("\tgoto LABEL_FALSE_");
            genCode(whileTemp);
            genCode("\n");
        }
        else if(strcmp($4,"Tint<")==0)
        {
            genCode("\tisub\n");
            genCode("\tiflt LABEL_TRUE_");
            genCode(whileTemp);
            genCode("\n");
            genCode("\tgoto LABEL_FALSE_");
            genCode(whileTemp);
            genCode("\n");
        }
        else if(strcmp($4,"Tint>=")==0)
        {
            genCode("\tisub\n");
            genCode("\tifge LABEL_TRUE_");
            genCode(whileTemp);
            genCode("\n");
            genCode("\tgoto LABEL_FALSE_");
            genCode(whileTemp);
            genCode("\n");
        }
        else if(strcmp($4,"Tint<=")==0)
        {
            genCode("\tisub\n");
            genCode("\tifle LABEL_TRUE_");
            genCode(whileTemp);
            genCode("\n");
            genCode("\tgoto LABEL_FALSE_");
            genCode(whileTemp);
            genCode("\n");
        }
        else if(strcmp($4,"Tint==")==0)
        {
            genCode("\tisub\n");
            genCode("\tifeq LABEL_TRUE_");
            genCode(whileTemp);
            genCode("\n");
            genCode("\tgoto LABEL_FALSE_");
            genCode(whileTemp);
            genCode("\n");
        }
        else if(strcmp($4,"Tint!=")==0)
        {
            genCode("\tisub\n");
            genCode("\tifne LABEL_TRUE_");
            genCode(whileTemp);
            genCode("\n");
            genCode("\tgoto LABEL_FALSE_");
            genCode(whileTemp);
            genCode("\n");
        }
        else if(strcmp($4,"Tfloat>")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tifgt LABEL_TRUE_");
            genCode(whileTemp);
            genCode("\n");
            genCode("\tgoto LABEL_FALSE_");
            genCode(whileTemp);
            genCode("\n");
        }
        else if(strcmp($4,"Tfloat<")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tiflt LABEL_TRUE_");
            genCode(whileTemp);
            genCode("\n");
            genCode("\tgoto LABEL_FALSE_");
            genCode(whileTemp);
            genCode("\n");
        }
        else if(strcmp($4,"Tfloat>=")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tifge LABEL_TRUE_");
            genCode(whileTemp);
            genCode("\n");
            genCode("\tgoto LABEL_FALSE_");
            genCode(whileTemp);
            genCode("\n");
        }
        else if(strcmp($4,"Tfloat<=")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tifle LABEL_TRUE_");
            genCode(whileTemp);
            genCode("\n");
            genCode("\tgoto LABEL_FALSE_");
            genCode(whileTemp);
            genCode("\n");
        }
        else if(strcmp($4,"Tfloat==")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tifeq LABEL_TRUE_");
            genCode(whileTemp);
            genCode("\n");
            genCode("\tgoto LABEL_FALSE_");
            genCode(whileTemp);
            genCode("\n");
        }
        else if(strcmp($4,"Tfloat!=")==0)
        {
            genCode("\tfcmpl\n");
            genCode("\tifne LABEL_TRUE_");
            genCode(whileTemp);
            genCode("\n");
            genCode("\tgoto LABEL_FALSE_");
            genCode(whileTemp);
            genCode("\n");
        }
        genCode("LABEL_TRUE_");
        genCode(whileTemp);
        genCode(":\n");
    } stat {
        char* whileTemp = (char*)malloc(10);
        sprintf(whileTemp, "%d", labelWhile_flag);
        genCode("\tgoto LABEL_BEGIN_");
        genCode(whileTemp);
        genCode("\n");

        genCode("LABEL_FALSE_");
        genCode(whileTemp);
        genCode(":\n");
        genCode("\tgoto EXIT_");
        genCode(whileTemp);
        genCode("\n");
        genCode("EXIT_");
        genCode(whileTemp);
        genCode(":\n");
        labelWhile_flag+=1;
    }
;

expression
    : assign_stat   {
        $$ = "";
    }
    | logic_stat    {
        $$ = $1;
    }
;

assign_stat
    : ID ASGN arithmetic_stat    {
        int i,flag=0;
        for(i=currentScope;i>=0;i--)
        {
            if(lookup_symbol($1, i, 0, "", "")==1)
                flag=1;
        }
        if(flag == 0)
        {
            sementicError("Undeclared variable", $1);
            dumpSementic = 1;
        }
        for(i=currentSymbolNumber-1;i>=0;i--)
        {
            if(strcmp(table[i].Name,$1)==0)
            {
                //printf("Index : %d  Scope : %d  Type : %s\n", table[i].Index, table[i].Scope, table[i].Type);
                if(table[i].Scope == 0)
                {
                    //global variable
                    if(strcmp(table[i].Type,"int")==0)
                    {
                        if(strcmp($3,"Tint")==0)
                        {
                            genCode("\tputstatic compiler_hw3/");
                            genCode(table[i].Name);
                            genCode(" I\n");
                        }
                        else if(strcmp($3,"Tfloat")==0)
                        {
                            genCode("\tf2i\n");
                            genCode("\tputstatic compiler_hw3/");
                            genCode(table[i].Name);
                            genCode(" I\n");
                        }
                    }
                    else if(strcmp(table[i].Type,"float")==0)
                    {
                        if(strcmp($3,"Tint")==0)
                        {
                            genCode("\ti2f\n");
                            genCode("\tputstatic compiler_hw3/");
                            genCode(table[i].Name);
                            genCode(" I\n");
                        }
                        else if(strcmp($3,"Tfloat")==0)
                        {
                            genCode("\tputstatic compiler_hw3/");
                            genCode(table[i].Name);
                            genCode(" F\n");
                        }
                    }
                    else if(strcmp(table[i].Type,"string")==0)
                    {
                        genCode(" [Ljava/lang/String;\n");
                    }
                    else if(strcmp(table[i].Type,"bool")==0)
                    {
                        genCode(" Z\n");
                    }
                }
                else
                {
                    if(strcmp(table[i].Type,"int")==0)
                    {
                        if(strcmp($3,"Tint")==0)
                        {
                            genCode("\tistore ");
                        }
                        else if(strcmp($3,"Tfloat")==0)
                        {
                            genCode("\tf2i\n");
                            genCode("\tistore ");
                        }
                    }
                    else if(strcmp(table[i].Type,"float")==0)
                    {
                        if(strcmp($3,"Tint")==0)
                        {
                            genCode("\ti2f\n");
                            genCode("\tfstore ");
                        }
                        else if(strcmp($3,"Tfloat")==0)
                        {
                            genCode("\tfstore ");
                        }
                    }
                    char* temp = (char*)malloc(10);
                    sprintf(temp,"%d",table[i].stackID);
                    genCode(temp);
                    genCode("\n");
                }
                break;
            }
        }
    }
    | ID asgn_op arithmetic_stat    {
        int i,flag=0;
        for(i=currentScope;i>=0;i--)
        {
            if(lookup_symbol($1, i, 0, "", "")==1)
                flag=1;
        }
        if(flag == 0)
        {
            sementicError("Undeclared variable", $1);
            dumpSementic = 1;
        }
        for(i=currentSymbolNumber-1;i>=0;i--)
        {
            if(strcmp(table[i].Name,$1)==0)
            {
                //printf("Index : %d  Scope : %d  Type : %s\n", table[i].Index, table[i].Scope, table[i].Type);
                if(table[i].Scope == 0)
                {
                    //global variable
                    if(strcmp(table[i].Type,"int")==0)
                    {
                        if(strcmp($3,"Tint")==0)
                        {
                            genCode("\tgetstatic compiler_hw3/");
                            genCode(table[i].Name);
                            genCode(" I\n");
                            if(strcmp($2,"+=")==0)
                            {
                                genCode("\tiadd\n");
                            }
                            else if(strcmp($2,"-=")==0)
                            {
                                genCode("\tisub\n");
                            }
                            else if(strcmp($2,"*=")==0)
                            {
                                genCode("\timul\n");
                            }
                            else if(strcmp($2,"/=")==0)
                            {
                                genCode("\tidiv\n");
                                if(loadZero_flag == 1)
                                {
                                    sementicError("divide by zero", "");
                                }
                            }
                            else if(strcmp($2,"%=")==0)
                            {
                                genCode("\tirem\n");
                            }
                            genCode("\tputstatic compiler_hw3/");
                            genCode(table[i].Name);
                            genCode(" I\n");
                        }
                        else if(strcmp($3,"Tfloat")==0)
                        {
                            genCode("\tgetstatic compiler_hw3/");
                            genCode(table[i].Name);
                            genCode(" I\n");
                            genCode("\ti2f\n");
                            if(strcmp($2,"+=")==0)
                            {
                                genCode("\tfadd\n");
                            }
                            else if(strcmp($2,"-=")==0)
                            {
                                genCode("\tfsub\n");
                            }
                            else if(strcmp($2,"*=")==0)
                            {
                                genCode("\tfmul\n");
                            }
                            else if(strcmp($2,"/=")==0)
                            {
                                genCode("\tfdiv\n");
                                if(loadZero_flag == 1)
                                {
                                    sementicError("divide by zero", "");
                                }
                            }
                            else if(strcmp($2,"%=")==0)
                            {
                                yyerror("float can not mod");
                            }
                            genCode("\tf2i\n");
                            genCode("\tputstatic compiler_hw3/");
                            genCode(table[i].Name);
                            genCode(" I\n");
                        }
                    }
                    else if(strcmp(table[i].Type,"float")==0)
                    {
                        if(strcmp($3,"Tint")==0)
                        {
                            genCode("\ti2f\n");
                            genCode("\tgetstatic compiler_hw3/");
                            genCode(table[i].Name);
                            genCode(" F\n");
                            if(strcmp($2,"+=")==0)
                            {
                                genCode("\tfadd\n");
                            }
                            else if(strcmp($2,"-=")==0)
                            {
                                genCode("\tfsub\n");
                            }
                            else if(strcmp($2,"*=")==0)
                            {
                                genCode("\tfmul\n");
                            }
                            else if(strcmp($2,"/=")==0)
                            {
                                genCode("\tfdiv\n");
                                if(loadZero_flag == 1)
                                {
                                    sementicError("divide by zero", "");
                                }
                            }
                            else if(strcmp($2,"%=")==0)
                            {
                                yyerror("float can not mod");
                            }
                            genCode("\tputstatic compiler_hw3/");
                            genCode(table[i].Name);
                            genCode(" F\n");
                        }
                        else if(strcmp($3,"Tfloat")==0)
                        {
                            genCode("\tgetstatic compiler_hw3/");
                            genCode(table[i].Name);
                            genCode(" F\n");
                            if(strcmp($2,"+=")==0)
                            {
                                genCode("\tfadd\n");
                            }
                            else if(strcmp($2,"-=")==0)
                            {
                                genCode("\tfsub\n");
                            }
                            else if(strcmp($2,"*=")==0)
                            {
                                genCode("\tfmul\n");
                            }
                            else if(strcmp($2,"/=")==0)
                            {
                                genCode("\tfdiv\n");
                                if(loadZero_flag == 1)
                                {
                                    sementicError("divide by zero", "");
                                }
                            }
                            else if(strcmp($2,"%=")==0)
                            {
                                yyerror("float can not mod");
                            }
                            genCode("\tputstatic compiler_hw3/");
                            genCode(table[i].Name);
                            genCode(" F\n");
                        }
                    }
                    else if(strcmp(table[i].Type,"string")==0)
                    {
                        genCode(" [Ljava/lang/String;\n");
                    }
                    else if(strcmp(table[i].Type,"bool")==0)
                    {
                        genCode(" Z\n");
                    }
                }
                else
                {
                    char* temp = (char*)malloc(10);
                    sprintf(temp,"%d",table[i].stackID);
                    if(strcmp(table[i].Type,"int")==0)
                    {
                        if(strcmp($3,"Tint")==0)
                        {
                            genCode("\tiload ");
                            genCode(temp);
                            genCode("\n");
                            if(strcmp($2,"+=")==0)
                            {
                                genCode("\tiadd\n");
                            }
                            else if(strcmp($2,"-=")==0)
                            {
                                genCode("\tisub\n");
                            }
                            else if(strcmp($2,"*=")==0)
                            {
                                genCode("\timul\n");
                            }
                            else if(strcmp($2,"/=")==0)
                            {
                                genCode("\tidiv\n");
                                if(loadZero_flag == 1)
                                {
                                    sementicError("divide by zero", "");
                                }
                            }
                            else if(strcmp($2,"%=")==0)
                            {
                                genCode("\tirem\n");
                            }
                            genCode("\tistore ");
                            genCode(temp);
                            genCode("\n");
                        }
                        else if(strcmp($3,"Tfloat")==0)
                        {
                            genCode("\tiload ");
                            genCode(temp);
                            genCode("\n");
                            genCode("\ti2f\n");
                            if(strcmp($2,"+=")==0)
                            {
                                genCode("\tfadd\n");
                            }
                            else if(strcmp($2,"-=")==0)
                            {
                                genCode("\tfsub\n");
                            }
                            else if(strcmp($2,"*=")==0)
                            {
                                genCode("\tfmul\n");
                            }
                            else if(strcmp($2,"/=")==0)
                            {
                                genCode("\tfdiv\n");
                                if(loadZero_flag == 1)
                                {
                                    sementicError("divide by zero", "");
                                }
                            }
                            else if(strcmp($2,"%=")==0)
                            {
                                yyerror("float can not mod");
                            }
                            genCode("\tf2i\n");
                            genCode("\tistore ");
                            genCode(temp);
                            genCode("\n");
                        }
                    }
                    else if(strcmp(table[i].Type,"float")==0)
                    {
                        if(strcmp($3,"Tint")==0)
                        {
                            genCode("\ti2f\n");
                            genCode("\tfload ");
                            genCode(temp);
                            genCode("\n");
                            if(strcmp($2,"+=")==0)
                            {
                                genCode("\tfadd\n");
                            }
                            else if(strcmp($2,"-=")==0)
                            {
                                genCode("\tfsub\n");
                            }
                            else if(strcmp($2,"*=")==0)
                            {
                                genCode("\tfmul\n");
                            }
                            else if(strcmp($2,"/=")==0)
                            {
                                genCode("\tfdiv\n");
                                if(loadZero_flag == 1)
                                {
                                    sementicError("divide by zero", "");
                                }
                            }
                            else if(strcmp($2,"%=")==0)
                            {
                                yyerror("float can not mod");
                            }
                            genCode("\tfstore ");
                            genCode(temp);
                            genCode("\n");
                        }
                        else if(strcmp($3,"Tfloat")==0)
                        {
                            genCode("\tfload ");
                            genCode(temp);
                            genCode("\n");
                            if(strcmp($2,"+=")==0)
                            {
                                genCode("\tfadd\n");
                            }
                            else if(strcmp($2,"-=")==0)
                            {
                                genCode("\tfsub\n");
                            }
                            else if(strcmp($2,"*=")==0)
                            {
                                genCode("\tfmul\n");
                            }
                            else if(strcmp($2,"/=")==0)
                            {
                                genCode("\tfdiv\n");
                                if(loadZero_flag == 1)
                                {
                                    sementicError("divide by zero", "");
                                }
                            }
                            else if(strcmp($2,"%=")==0)
                            {
                                yyerror("float can not mod");
                            }
                            genCode("\tfstore ");
                            genCode(temp);
                            genCode("\n");
                        }
                    }
                }
                break;
            }
        }
        loadZero_flag = 0;
    }
    
;

arithmetic_stat
    : arithmetic_stat add_op arithmetic_temp1   {
        if(strcmp($1,"Tint")==0 && strcmp($3,"Tint")==0)
        {
            if(strcmp($2,"+")==0)
            {
                genCode("\tiadd\n");
            }
            else if(strcmp($2,"-")==0)
            {
                genCode("\tisub\n");
            }
            $$ = "Tint";
        }
        else if(strcmp($1,"Tfloat")==0 && strcmp($3,"Tfloat")==0)
        {
            if(strcmp($2,"+")==0)
            {
                genCode("\tfadd\n");
            }
            else if(strcmp($2,"-")==0)
            {
                genCode("\tfsub\n");
            }
            $$ = "Tfloat";
        }
        else if(strcmp($1,"Tint")==0 && strcmp($3,"Tfloat")==0)
        {
            genCode("\tswap\n");
            genCode("\ti2f\n");
            genCode("\tswap\n");
            if(strcmp($2,"+")==0)
            {
                genCode("\tfadd\n");
            }
            else if(strcmp($2,"-")==0)
            {
                genCode("\tfsub\n");
            }
            $$ = "Tfloat";
        }
        else if(strcmp($1,"Tfloat")==0 && strcmp($3,"Tint")==0)
        {
            genCode("\ti2f\n");
            if(strcmp($2,"+")==0)
            {
                genCode("\tfadd\n");
            }
            else if(strcmp($2,"-")==0)
            {
                genCode("\tfsub\n");
            }
            $$ = "Tfloat";
        }
    }
    | arithmetic_temp1  {
        $$ = $1;
    }
;
arithmetic_temp1
    : arithmetic_temp1 mul_op arithmetic_temp2  {
        if(strcmp($1,"Tint")==0 && strcmp($3,"Tint")==0)
        {
            if(strcmp($2,"*")==0)
            {
                genCode("\timul\n");
            }
            else if(strcmp($2,"/")==0)
            {
                genCode("\tidiv\n");
                if(loadZero_flag == 1)
                {
                    sementicError("divide by zero", "");
                }
            }
            else if(strcmp($2,"%")==0)
            {
                genCode("\tirem\n");
            }
            $$ = "Tint";
        }
        else if(strcmp($1,"Tfloat")==0 && strcmp($3,"Tfloat")==0)
        {
            if(strcmp($2,"*")==0)
            {
                genCode("\tfmul\n");
            }
            else if(strcmp($2,"/")==0)
            {
                genCode("\tfdiv\n");
                if(loadZero_flag == 1)
                {
                    sementicError("divide by zero", "");
                }
            }
            else if(strcmp($2,"%")==0)
            {
                yyerror("float can not use mod");
            }
            $$ = "Tfloat";
        }
        else if(strcmp($1,"Tint")==0 && strcmp($3,"Tfloat")==0)
        {
            genCode("\tswap\n");
            genCode("\ti2f\n");
            genCode("\tswap\n");
            if(strcmp($2,"*")==0)
            {
                genCode("\tfmul\n");
            }
            else if(strcmp($2,"/")==0)
            {
                genCode("\tfdiv\n");
                if(loadZero_flag == 1)
                {
                    sementicError("divide by zero", "");
                }
            }
            else if(strcmp($2,"%")==0)
            {
                yyerror("float can not use mod");
            }
            $$ = "Tfloat";
        }
        else if(strcmp($1,"Tfloat")==0 && strcmp($3,"Tint")==0)
        {
            genCode("\ti2f\n");
            if(strcmp($2,"*")==0)
            {
                genCode("\tfmul\n");
            }
            else if(strcmp($2,"/")==0)
            {
                genCode("\tfdiv\n");
                if(loadZero_flag == 1)
                {
                    sementicError("divide by zero", "");
                }
            }
            else if(strcmp($2,"%")==0)
            {
                yyerror("float can not use mod");
            }
            $$ = "Tfloat";
        }
        loadZero_flag = 0;
    }
    | arithmetic_temp2  {
        $$ = $1;
    }
;
arithmetic_temp2
    : LB expression RB  { $$ = $2; }
    | id_or_init    {
        $$ = $1;
    }
;
id_or_init
    : ID    {
        int i,flag=0;
        for(i=currentScope;i>=0;i--)
        {
            if(lookup_symbol($1, i, 0, "", "")==1)
                flag=1;
        }
        if(flag == 0)
        {
            sementicError("Undeclared variable", $1);
            dumpSementic = 1;
        }

        for(i=currentSymbolNumber-1;i>=0;i--)
        {
            if(strcmp(table[i].Name,$1)==0)
            {
                //printf("Index : %d  Scope : %d  Type : %s\n", table[i].Index, table[i].Scope, table[i].Type);
                if(table[i].Scope == 0)
                {
                    //global variable
                    genCode("\tgetstatic compiler_hw3/");
                    genCode(table[i].Name);
                    if(strcmp(table[i].Type,"int")==0)
                    {
                        genCode(" I\n");
                        $$ = "Tint";
                    }
                    else if(strcmp(table[i].Type,"float")==0)
                    {
                        genCode(" F\n");
                        $$ = "Tfloat";
                    }
                    else if(strcmp(table[i].Type,"string")==0)
                    {
                        genCode(" [Ljava/lang/String;\n");
                        $$ = "Tstring";
                    }
                    else if(strcmp(table[i].Type,"bool")==0)
                    {
                        genCode(" Z\n");
                        $$ = "Tbool";
                    }
                }
                else
                {
                    if(strcmp(table[i].Type,"int")==0)
                    {
                        genCode("\tiload ");
                        $$ = "Tint";
                    }
                    else if(strcmp(table[i].Type,"float")==0)
                    {
                        genCode("\tfload ");
                        $$ = "Tfloat";
                    }
                    else if(strcmp(table[i].Type,"string")==0)
                    {
                        genCode("\tiload ");
                        $$ = "Tstring";
                    }
                    else if(strcmp(table[i].Type,"bool")==0)
                    {
                        genCode("\tiload ");
                        $$ = "Tbool";
                    }
                    char* temp = (char*)malloc(10);
                    sprintf(temp,"%d",table[i].stackID);
                    genCode(temp);
                    genCode("\n");
                }
                break;
            }
        }
    }
    | ID inc_op {
        int i,flag=0;
        for(i=currentScope;i>=0;i--)
        {
            if(lookup_symbol($1, i, 0, "", "")==1)
                flag=1;
        }
        if(flag == 0)
        {
            sementicError("Undeclared variable", $1);
            dumpSementic = 1;
        }

        for(i=currentSymbolNumber-1;i>=0;i--)
        {
            if(strcmp(table[i].Name,$1)==0)
            {
                //printf("Index : %d  Scope : %d  Type : %s\n", table[i].Index, table[i].Scope, table[i].Type);
                if(table[i].Scope == 0)
                {
                    //global variable
                    genCode("\tgetstatic compiler_hw3/");
                    genCode(table[i].Name);
                    if(strcmp(table[i].Type,"int")==0)
                    {
                        genCode(" I\n");
                        genCode("\tldc 1\n");
                        if(strcmp($2,"++")==0)
                        {
                            genCode("\tiadd\n");
                        }
                        else if(strcmp($2,"--")==0)
                        {
                            genCode("\tisub\n");
                        }
                        genCode("\tputstatic compiler_hw3/");
                        genCode(table[i].Name);
                        genCode(" I\n");
                        $$ = "Tint";
                    }
                    else if(strcmp(table[i].Type,"float")==0)
                    {
                        yyerror("float can not ++/--");
                    }
                    
                }
                else
                {
                    if(strcmp(table[i].Type,"int")==0)
                    {
                        genCode("\tiload ");
                        char* temp = (char*)malloc(10);
                        sprintf(temp,"%d",table[i].stackID);
                        genCode(temp);
                        genCode("\n");
                        genCode("\tldc 1\n");
                        if(strcmp($2,"++")==0)
                        {
                            genCode("\tiadd\n");
                        }
                        else if(strcmp($2,"--")==0)
                        {
                            genCode("\tisub\n");
                        }
                        genCode("\tistore ");
                        genCode(temp);
                        genCode("\n");
                        $$ = "Tint";
                    }
                    else if(strcmp(table[i].Type,"float")==0)
                    {
                        yyerror("float can not ++/--");
                    }
                    
                }
                break;
            }
        }
    }
    | function_call
    | initializer   {
        $$ = $1;
    }
;

compare_stat
    : arithmetic_stat comp_op arithmetic_stat   {
        if(strcmp($1,"Tint")==0 && strcmp($3,"Tint")==0)
        {
            char* temp = (char*)malloc(10);
            sprintf(temp, "Tint%s", $2);
            $$ = temp;
        }
        else if(strcmp($1,"Tfloat")==0 && strcmp($3,"Tfloat")==0)
        {
            char* temp = (char*)malloc(10);
            sprintf(temp, "Tfloat%s", $2);
            $$ = temp;
        }
    }
;

logic_stat
    : logic_stat OR logic_temp1
    | logic_temp1   {
        $$ = $1;
    }
;
logic_temp1
    : logic_temp1 AND logic_temp2
    | logic_temp2   {
        $$ = $1;
    }
;
logic_temp2
    : NOT logic_temp3   { $$ = ""; }
    | logic_temp3       {
        $$ = $1;
    }
;
logic_temp3
    : logics    {
        $$ = $1;
    }
;
logics
    : arithmetic_stat   {
        $$ = $1;
    }
    | compare_stat      { $$ = $1; }
;

add_op
    : ADD   { $$ = "+"; }
    | SUB   { $$ = "-"; }
;

mul_op
    : MUL   { $$ = "*"; }
    | DIV   { $$ = "/"; }
    | MOD   { $$ = "%"; }
;

inc_op
    : INC   { $$ = "++"; }
    | DEC   { $$ = "--"; }
;

comp_op
    : MT    { $$ = ">"; }
    | LT    { $$ = "<"; }
    | MTE   { $$ = ">="; }
    | LTE   { $$ = "<="; }
    | EQ    { $$ = "=="; }
    | NE    { $$ = "!="; }
;

asgn_op
    : ADDASGN   { $$ = "+="; }
    | SUBASGN   { $$ = "-="; }
    | MULASGN   { $$ = "*="; }
    | DIVASGN   { $$ = "/="; }
    | MODASGN   { $$ = "%="; }
;

function_call
    : ID LB function_call_parameter_list RB   {

        // printf("(%s)", $3);
        
        int i,flag=0;
        for(i=currentScope;i>=0;i--)
        {
            if(lookup_symbol($1, i, 0, "", "")==1)
                flag=1;
        }
        if(flag == 0)
        {
            sementicError("Undeclared function", $1);
            dumpSementic = 1;
        }

        for(i=currentSymbolNumber-1;i>=0;i--)
        {
            if(strcmp(table[i].Name,$1)==0)
            {
                //printf("Index : %d  Name : %s  Scope : %d  Type : %s\n", table[i].Index, table[i].Name, table[i].Scope, table[i].Type);
                genCode("\tinvokestatic compiler_hw3/");
                genCode(table[i].Name);
                genCode("(");
                char* attrTemp = (char*)malloc(100);
                strcpy(attrTemp,table[i].Attribute);

                // printf("((%s))", attrTemp);
                if(strcmp($3, attrTemp)!=0)
                {
                    sementicError("function formal parameter is not the same", "");
                }

                char* pch;
                char *saveptrPch = NULL;
                pch = strtok_r(attrTemp,", ",&saveptrPch);
                while (pch != NULL)
                {
                    if(strcmp(pch,"int")==0)
                    {
                        genCode("I");
                    }
                    else if(strcmp(pch,"float")==0)
                    {
                        genCode("F");
                    }
                    else if(strcmp(pch,"string")==0)
                    {
                        genCode("Ljava/lang/String;");
                    }
                    else if(strcmp(pch,"bool")==0)
                    {
                        genCode("Z");
                    }
                    else if(strcmp(pch,"void")==0)
                    {
                        genCode("V");
                    }
                    pch = strtok_r(NULL, ", ",&saveptrPch);
                }
                genCode(")");
                if(strcmp(table[i].Type,"int")==0)
                {
                    genCode("I\n");
                    $$ = "Tint";
                }
                else if(strcmp(table[i].Type,"float")==0)
                {
                    genCode("F\n");
                    $$ = "Tfloat";
                }
                else if(strcmp(table[i].Type,"string")==0)
                {
                    genCode("Ljava/lang/String;\n");
                    $$ = "Tstring";
                }
                else if(strcmp(table[i].Type,"bool")==0)
                {
                    genCode("Z\n");
                    $$ = "Tbool";
                }
                else if(strcmp(table[i].Type,"void")==0)
                {
                    genCode("V\n");
                    $$ = "Tvoid";
                }
            }
        }
    }
;
function_call_parameter_list
    : function_call_parameter function_call_parameters  {
        if(strlen($2)>0){
            char* funParaTemp = (char*)malloc(100);
            sprintf(funParaTemp, "%s, %s", $1, $2);
            $$ = funParaTemp;
            //printf("(%s)(%s)(%s)",$1, $2, funParaTemp);
        }
        else{
            $$=$1;
        }
    }
    |   { $$ = ""; }
;
function_call_parameters
    : COMMA function_call_parameter_list    { $$ = $2; }
    |   { $$ = ""; }
;
function_call_parameter
    : expression    { 
        if(strcmp($1, "Tint")==0)
        {
            $$ = "int";
        }
        else if(strcmp($1, "Tfloat")==0)
        {
            $$ = "float";
        }
        else if(strcmp($1,"Tstring")==0)
        {
            $$ = "string";
        }
        else if(strcmp($1,"Tbool")==0)
        {
            $$ = "bool";
        }
    }
;


print_func
    : PRINT LB print_stat RB SEMICOLON  {
        // genCode("\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
        // genCode("\tswap\n");
        // genCode("invokevirtual java/io/PrintStream/println(");

    }
;

print_stat
    : ID    {
        int i,flag=0;
        for(i=currentScope;i>=0;i--)
        {
            if(lookup_symbol($1, i, 0, "", "")==1)
                flag=1;
        }
        if(flag == 0)
        {
            sementicError("Undeclared variable", $1);
            dumpSementic = 1;
        }

        for(i=currentSymbolNumber-1;i>=0;i--)
        {
            if(strcmp(table[i].Name,$1)==0)
            {
                //printf("Index : %d  Scope : %d  Type : %s\n", table[i].Index, table[i].Scope, table[i].Type);
                if(table[i].Scope == 0)
                {
                    //global variable
                    genCode("\tgetstatic compiler_hw3/");
                    genCode(table[i].Name);
                    if(strcmp(table[i].Type,"int")==0)
                    {
                        genCode(" I\n");
                    }
                    else if(strcmp(table[i].Type,"float")==0)
                    {
                        genCode(" F\n");
                    }
                    else if(strcmp(table[i].Type,"string")==0)
                    {
                        genCode(" Ljava/lang/String;\n");
                    }
                    else if(strcmp(table[i].Type,"bool")==0)
                    {
                        genCode(" Z\n");
                    }
                    genCode("\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
                    genCode("\tswap\n");
                    if(strcmp(table[i].Type,"int")==0)
                    {
                        genCode("\tinvokevirtual java/io/PrintStream/println(I)V\n");
                    }
                    else if(strcmp(table[i].Type,"float")==0)
                    {
                        genCode("\tinvokevirtual java/io/PrintStream/println(F)V\n");
                    }
                    else if(strcmp(table[i].Type,"string")==0)
                    {
                        genCode("\tinvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
                    }
                }
                else
                {
                    if(strcmp(table[i].Type,"int")==0)
                    {
                        genCode("\tiload ");
                    }
                    else if(strcmp(table[i].Type,"float")==0)
                    {
                        genCode("\tfload ");
                    }
                    else if(strcmp(table[i].Type,"string")==0)
                    {
                        genCode("\taload ");
                    }
                    else if(strcmp(table[i].Type,"bool")==0)
                    {
                        genCode("\tiload ");
                    }
                    char* temp = (char*)malloc(10);
                    sprintf(temp,"%d",table[i].stackID);
                    genCode(temp);
                    genCode("\n");

                    genCode("\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
                    genCode("\tswap\n");
                    if(strcmp(table[i].Type,"int")==0)
                    {
                        genCode("\tinvokevirtual java/io/PrintStream/println(I)V\n");
                    }
                    else if(strcmp(table[i].Type,"float")==0)
                    {
                        genCode("\tinvokevirtual java/io/PrintStream/println(F)V\n");
                    }
                    else if(strcmp(table[i].Type,"string")==0)
                    {
                        genCode("\tinvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
                    }
                }
                break;
            }
        }
                        }
    | initializer   {
        genCode("\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
        genCode("\tswap\n");
        if(strcmp($1,"Tint")==0)
        {
            genCode("\tinvokevirtual java/io/PrintStream/println(I)V\n");
        }
        else if(strcmp($1,"Tfloat")==0)
        {
            genCode("\tinvokevirtual java/io/PrintStream/println(F)V\n");
        }
        else if(strcmp($1,"Tstring")==0)
        {
            genCode("\tinvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
        }
    }
;

/* actions can be taken when meet the token or rule */
type
    : INT           { $$ = $1; }
    | FLOAT         { $$ = $1; }
    | BOOL          { $$ = $1; }
    | TSTRING       { $$ = $1; }
    | VOID          { $$ = $1; }
;

%%

/* C code section */
int main(int argc, char** argv)
{
    yylineno = 0;
    
    sementicErrorList[0] = (char*)malloc(200);
    sementicErrorList[1] = (char*)malloc(200);
    sementicErrorList[2] = (char*)malloc(200);
    sementicErrorList[3] = (char*)malloc(200);


    file = fopen("compiler_hw3.j","w");

    fprintf(file, ".class public compiler_hw3\n.super java/lang/Object\n");


    yyparse();
    
    fclose(file);

    if(shutDown == 0)
    {
        if(strlen(buf)!=0)
        {
            yylineno+=1;
            if(strcmp(buf,"\n")==0)
            {
                printf("%d:", yylineno);
            }
            else
                printf("%d: ", yylineno);
            printf("%s",buf);
        }
        int i;
        for(i = currentScope; i>=0; i--)
            dump_symbol(i);
        printf("\nTotal lines: %d \n",yylineno);
        if(errorr_flag == 1)
        {
            remove("compiler_hw3.j");
        }
    }
    else if(errorr_flag==1)
    {
        remove("compiler_hw3.j");
    }


    free(sementicErrorList[0]);
    free(sementicErrorList[1]);
    free(sementicErrorList[2]);
    free(sementicErrorList[3]);

    return 0;
}

void yyerror(char *s)
{
    errorr_flag = 1;
    dumpSyntax = 1;
    
    // keepScan();
    yylineno+=1;
    if(strcmp(buf,"\n")==0)
    {
        printf("%d:", yylineno);
    }
    else
    {
        printf("%d: ", yylineno);
    }
    printf("%s", buf);
    

    if(dumpSementic == 1)
    {
        sprintf(sementicErrorList[1],"%s%s\n",sementicErrorList[1], buf);
        printf("\n%s", sementicErrorList[0]);
        printf("%s", sementicErrorList[1]);
        printf("%s", sementicErrorList[2]);
        printf("%s", sementicErrorList[3]);
        dumpSementic = 0;
    }
    
    printf("\n|-----------------------------------------------|\n");
    printf("| Error found in line %d: %s\n", yylineno, buf);
    printf("| %s", s);
    printf("\n|-----------------------------------------------|\n\n");

    shutDown = 1;
}

void sementicError(char* s, char* variable)
{
    errorr_flag = 1;
    strcpy(sementicErrorList[0], "\n|-----------------------------------------------|\n");
    sprintf(sementicErrorList[1], "| Error found in line %d: ", yylineno+1);
    sprintf(sementicErrorList[2], "| %s %s", s, variable);
    strcpy(sementicErrorList[3], "\n|-----------------------------------------------|\n\n");

    dumpSementic = 1;
}

void create_symbol(int x, int entry) {
    //create symbol table
    symbolTablesIndex[x] = entry;
}
void insert_symbol(int Index, char* Name,char* Kind, char* Type, int Scope, char* Attribute, int functionForward, int stackID) {
    // check exist or not
    int check = 0;
    check = lookup_symbol(Name, Scope, functionForward, Attribute, Type);
    if(check == 1)
    {
        //semantic error
        char* errorMessage = (char*)malloc(100);
        strcpy(errorMessage, "");
        strcat(errorMessage, "Redeclared ");
        strcat(errorMessage, Kind);
        
        sementicError(errorMessage, Name);
        
    }
    else if(check == 2)
    {
        //function forward declaration, no need to insert
        return;
    }
    else if(check == 3)
    {
        //re function forward declaration, sementic error
        char* errorMessage = (char*)malloc(100);
        strcpy(errorMessage, "");
        strcat(errorMessage, "Redeclared ");
        strcat(errorMessage, Kind);
        
        sementicError(errorMessage, Name);
    }
    else if(check == 0)
    {
        //not exist, and insert
        table[currentSymbolNumber].Index = Index;
        table[currentSymbolNumber].Name = Name;
        table[currentSymbolNumber].Kind = Kind;
        table[currentSymbolNumber].Type = Type;
        table[currentSymbolNumber].Scope = Scope;
        table[currentSymbolNumber].Attribute = Attribute;
        table[currentSymbolNumber].functionForward = functionForward;
        table[currentSymbolNumber].stackID=stackID;
        table[currentSymbolNumber].Ivalue = 0;
        table[currentSymbolNumber].Fvalue = 0;
        table[currentSymbolNumber].Svalue = "";

        currentSymbolNumber += 1;
    }
    return;
}
int lookup_symbol(char* Name, int Scope, int Funforward, char* Attribute, char* Type) {
    int i;
    for(i=0;i<currentSymbolNumber;i++)
    {
        if(strcmp(table[i].Name, Name) == 0 && table[i].Scope == Scope)
        {
            if(table[i].functionForward == 0)
            {
                //exist, sementic error
                return 1;
            }
            else if(table[i].functionForward == 1 && Funforward == 1)
            {
                //re function forward declaration, sementic error
                return 3;
            }
            else if(table[i].functionForward == 1 && Funforward == 0)
            {
                //function declaration
                if(strcmp(table[i].Attribute, Attribute)==0 && strcmp(table[i].Type, Type)==0)
                {
                    //same return type, and same formal parameters
                    table[i].functionForward = 0;
                    return 2;
                }
                else if(strcmp(table[i].Attribute, Attribute)!=0 && strcmp(table[i].Type, Type)==0)
                {
                    //same return type, but different formal attributes
                    sementicError("function formal parameter is not the same", "");
                    return 2;
                }
                else if(strcmp(table[i].Attribute, Attribute)==0 && strcmp(table[i].Type, Type)!=0)
                {
                    //different return type, but same formal attributes
                    sementicError("function return type is not the same", "");
                    return 2;
                }
                else if(strcmp(table[i].Attribute, Attribute)!=0 && strcmp(table[i].Type, Type)!=0)
                {
                    //different return type, and different formal attributes 
                    sementicError("1. function return type is not the same\n| 2. function formal parameter is not the same", "");
                    return 2;
                }
                
            }
        }
    }
    return 0;
}
void dump_symbol(int currentScope) {

    int i, flag=0;
    for(i=0;i<currentSymbolNumber;i++)
    {
        if(table[i].Scope == currentScope)
        {
            if(flag == 0)
            {
                // printf("\n%-10s%-10s%-12s%-10s%-10s%-10s%-10s%-10s%-10s%-10s%-10s\n\n","Index", "Name", "Kind", "Type", "Scope", "Attribute", "Funfoward", "stackID", "Ivalue", "Fvalue", "Svalue");
                printf("\n%-10s%-10s%-12s%-10s%-10s%-10s\n\n","Index", "Name", "Kind", "Type", "Scope", "Attribute");
                flag = 1;
            }

            //printf("%-10d%-10s%-12s%-10s%-10d%-10s%-10d%-10d%-10d%-10f%-10s\n",table[i].Index, table[i].Name, table[i].Kind, table[i].Type, table[i].Scope, table[i].Attribute, table[i].functionForward, table[i].stackID, table[i].Ivalue, table[i].Fvalue, table[i].Svalue);
            printf("%-10d%-10s%-12s%-10s%-10d%s\n",table[i].Index, table[i].Name, table[i].Kind, table[i].Type, table[i].Scope, table[i].Attribute);
            // if(strcmp(table[i].Attribute,"")!=0)
            //     printf("%-10s\n",table[i].Attribute);
            // else
            //     printf("\n");
            table[i].Scope = -1;
        }
    }
    if(flag==1)
        printf("\n");
    symbolTablesIndex[currentScope]=0;
    return;
}

void genCode(char *code)
{
    fprintf(file, "%s", code);
}