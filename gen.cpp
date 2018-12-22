#include <stdio.h>
#include <stdlib.h> // exit ()
#include <stdarg.h>
#include <stack>

#include "ast.h"
#include "symtab.h"

/*    This stack is used to implement  break statements.
      "break"  is implemented as a goto to the exit label of the most closely enclosing loop (or switch statement). 
      This label is the top of the stack. Below it on the stack is the exit label of the enclosing loop and so on.
      The stack is empty when we are not inside a  loop (or switch statement).
*/
static	  
std::stack<int> exitlabels;

static
int newTemp ()
{
   static int counter = 1;
   return counter++;
} 

static
int newlabel ()
{
   static int counter = 1;
   return counter++;
} 

// emit works just like  printf  --  we use emit 
// to generate code and print it to the standard output.
void emit (const char *format, ...)
{
    // if (errors > 0) return; // do not generate code if there are errors.  This should be controlled by if !defined (DEBUG)) 

    printf ("    ");  // this is meant to add a nice indentation.
                      // Use emitlabel() to print a label without the indentation.    
    va_list argptr;
	va_start (argptr, format);
	// all the arguments following 'format' are passed on to vprintf
	vprintf (format, argptr); 
	va_end (argptr);
}

/* use this  to emit a label without using indentation */
void emitlabel (int label) 
{
    printf ("label%d:\n",  label);
}    

/* there are two versions of each arithmetic operator in the
   generated code. One is  used for  operands having type int. 
   The other is used for operands having type float.
*/   
struct operator_names { 
    const char *int_name; 
	const char *float_name; 
};

static
struct operator_names 
opNames [] = { {"+", "@+"},
            { "-", "@-"},
			{ "*", "@*" },
			{ "/", "@/" }};

/* convert operator  to  string  suitable for the given type
  e.g. opName (PLUS, _INT)  returns "+"
       opName (PLUS, _FLOAT) returns  "@+"
*/
const char *
opName (enum op op, myType t)
{
    if (op > DIV) { fprintf (stderr, "internal compiler error #1"); exit (1); }
    if (t == _INT)
	    return opNames [op].int_name;
	else
	    return opNames [op].float_name;
}

int BinaryOp::genExp ()
{
    if (_left->_type != _right->_type)
      return -1;
	 	
	int left_operand_result = _left->genExp ();
	int right_operand_result = _right->genExp ();
	
	int result = newTemp ();
	
	const char *the_op = opName (_op, _type);

  	emit ("_t%d = _t%d %s _t%d\n", result, left_operand_result, the_op, right_operand_result);
	return result;
}

int NumNode::genExp () 
{
    int result = newTemp ();
	if (_type == _INT)
  	    emit ("_t%d = %d\n", result, _u.ival);
	else
	    emit ("_t%d = %.2f\n", result, _u.fval);
	return result;
}

int IdNode::genExp ()
{
    int result = newTemp ();
		
	emit ("_t%d = %s\n", result, _name);
	return result;
}

void SimpleBoolExp::genBoolExp (int truelabel, int falselabel)
{
    if (truelabel == FALL_THROUGH && falselabel == FALL_THROUGH)
	    return; // no need for code 

    const char *the_op;
	
	int left_result = _left->genExp ();
	int right_result = _right->genExp ();
	
    switch (_op) {
	    case LT:
		    the_op = "<";
			break;
		case GT:
		    the_op = ">";
			break;
		case LE:
		    the_op = "<=";
			break;
		case GE:
		    the_op = ">=";
			break;
		case EQ:
		    the_op = "==";
			break;
		case NE:
		    the_op = "!=";
			break;
		default:
		    fprintf (stderr, "internal compiler error #3\n"); exit (1);
	}
	
	if  (truelabel == FALL_THROUGH)
   	    emit ("ifFalse _t%d %s _t%d goto label%d\n", left_result, the_op, right_result, falselabel);
    else if (falselabel == FALL_THROUGH)
   	    emit ("if _t%d %s _t%d goto label%d\n", left_result, the_op, right_result, truelabel);
	else { // no fall through
  	    emit ("if _t%d %s _t%d goto label%d\n", left_result, the_op, right_result, truelabel);
	    emit ("goto label%d\n", falselabel);
	}
}

void Or::genBoolExp (int truelabel, int falselabel)
{
    if (truelabel == FALL_THROUGH && falselabel == FALL_THROUGH)
	    return; // no need for code 

    if  (truelabel == FALL_THROUGH) {
	    int next_label = newlabel(); // FALL_THROUGH implemented by jumping to next_label
	    _left->genBoolExp (next_label, // if left operand is true then the OR expression
		                               //is true so jump to next_label (thus falling through
									   // to the code following the code for the OR expression)
		                   FALL_THROUGH); // if left operand is false then 
						                  // fall through and evaluate right operand   
		_right->genBoolExp (FALL_THROUGH, falselabel);
        emitlabel (next_label);
    }  else if (falselabel == FALL_THROUGH) {
       _left->genBoolExp (truelabel, // if left operand is true then the OR expresson is true 
	                                 // so jump to  truelabel (without evaluating right operand)
                          FALL_THROUGH); // if left operand is false then 
						                  // fall through and evaluate right operand
	   _right->genBoolExp (truelabel, FALL_THROUGH);
	} else { // no fall through
	   _left->genBoolExp (truelabel, // if left operand is true then the or expresson is true 
	                                 // so jump to  truelabel (without evaluating right operand)
						  FALL_THROUGH); // if left operand is false then 
						                  // fall through and evaluate right operand
	   _right->genBoolExp (truelabel, falselabel);
	}
}

void And::genBoolExp (int truelabel, int falselabel)
{
    if (truelabel == FALL_THROUGH && falselabel == FALL_THROUGH)
	    return; // no need for code 
		
	if  (truelabel == FALL_THROUGH) {
	    _left->genBoolExp (FALL_THROUGH, // if left operand is true then fall through and evaluate
		                                 // right operand.
                           falselabel); // if left operand is false then the AND expression is
                                        // false so jump to falselabel);
        _right->genBoolExp (FALL_THROUGH, falselabel);
    } else if (falselabel == FALL_THROUGH) {
	    int next_label = newlabel(); // FALL_THROUGH implemented by jumping to next_label
        _left->genBoolExp (FALL_THROUGH, // if left operand is true then fall through and
                                         // evaluate right operand
                           next_label); // if left operand is false then the AND expression 
                                        //  is false so jump to next_label (thus falling through to
                                        // the code following the code for the AND expression)
        _right->genBoolExp (truelabel, FALL_THROUGH);
		emitlabel(next_label);
    } else { // no fall through
        _left->genBoolExp (FALL_THROUGH, 	// if left operand is true then fall through and
                                         // evaluate right operand
						   falselabel); // if left operand is false then the AND expression is false
						                // so jump to falselabel (without evaluating the right operand)
		_right->genBoolExp (truelabel, falselabel);
	}
}

void Not::genBoolExp (int truelabel, int falselabel)
{
    _operand->genBoolExp (falselabel, truelabel); 
}

void ReadStmt::genStmt()
{
	myType idtype = _id->_type; 
	
	if (idtype == _INT)
 	  emit ("iread %s\n", _id->_name);
    else
      emit ("fread %s\n", _id->_name);
}

void AssignStmt::genStmt()
{
    int result = _rhs->genExp();
	
	myType idtype = _lhs->_type; 
	
	if (idtype == _rhs->_type)
	  emit ("%s = _t%d\n", _lhs->_name, result);
}


void IfStmt::genStmt()
{
    int elseStmtlabel = newlabel ();
	int exitlabel = newlabel ();
	
	_condition->genBoolExp (FALL_THROUGH, elseStmtlabel);
	
    _thenStmt->genStmt ();
	emit ("goto label%d\n", exitlabel);
	emitlabel(elseStmtlabel);
    _elseStmt->genStmt();
	emitlabel(exitlabel);
}

void WhileStmt::genStmt()
{
    int condlabel = newlabel ();
	int exitlabel = newlabel ();
	
	emitlabel(condlabel);
	_condition->genBoolExp (FALL_THROUGH, exitlabel);
	
	
	
	_body->genStmt ();
	
	
	emit ("goto label%d\n", condlabel);
	emitlabel(exitlabel);
}

void Block::genStmt()
{
    for (Stmt *stmt = _stmtlist; stmt != NULL; stmt = stmt->_next)
	    stmt->genStmt();
}

void SwitchStmt::genStmt()
{ 
    /* not implemented yet; */
}

void BreakStmt::genStmt()
{
	/* not implemented yet; */	
}



