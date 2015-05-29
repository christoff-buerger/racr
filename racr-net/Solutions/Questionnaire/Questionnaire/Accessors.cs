/*
 This program and the accompanying materials are made available under the
 terms of the MIT license (X11 license) which accompanies this distribution.

 Author: D. Langner, C. Bürger
*/

using System.Windows.Forms;

static class Accessors {
	// AST Accessors
	public static Racr.AstNode GetBody(this Racr.AstNode n) { return n.Child("Body"); }
	public static Racr.AstNode GetExpression(this Racr.AstNode n) { return n.Child("Expression"); }
	public static string GetName(this Racr.AstNode n) { return n.Child<string>("name"); }
	public static string GetLabel(this Racr.AstNode n) { return n.Child<string>("label"); }
	public static ValueTypes GetValueType(this Racr.AstNode n) { return n.Child<ValueTypes>("type"); }
	public static object GetValue(this Racr.AstNode n) { return n.Child<object>("value"); }
	public static Racr.AstNode GetOperands(this Racr.AstNode n) { return n.Child("Operands"); }
	public static string GetOperator(this Racr.AstNode n) { return n.Child<string>("operator"); }

	// Attribute Accessors
	public static Racr.AstNode Root(this Racr.AstNode n) { return n.AttValue<Racr.AstNode>("Root"); }
	public static Racr.AstNode ErrorQuestion(this Racr.AstNode n) { return n.AttValue<Racr.AstNode>("ErrorQuestion"); }
	public static Racr.AstNode GLookup(this Racr.AstNode n, string name) { return n.AttValue<Racr.AstNode>("GLookup", name); }
	public static Racr.AstNode LLookup(this Racr.AstNode n, string name) { return n.AttValue<Racr.AstNode>("LLookup", name); }
	public static Racr.AstNode FindActive(this Racr.AstNode n, string name) { return n.AttValue<Racr.AstNode>("FindActive", name); }
	public static bool IsErrorQuestion(this Racr.AstNode n) { return n.AttValue<bool>("IsErrorQuestion"); }
	public static bool IsValid(this Racr.AstNode n) { return n.AttValue<bool>("IsValid"); }
	public static bool IsLValid(this Racr.AstNode n) { return n.AttValue<bool>("IsLValid"); }
	public static bool IsActive(this Racr.AstNode n) { return n.AttValue<bool>("IsActive"); }
	public static bool IsShown(this Racr.AstNode n) { return n.AttValue<bool>("IsShown"); }
	public static ValueTypes Type(this Racr.AstNode n) { return n.AttValue<ValueTypes>("Type"); }
	public static object Value(this Racr.AstNode n) { return n.AttValue<object>("Value"); }
	public static Control Widget(this Racr.AstNode n) { return n.AttValue<Control>("Widget"); }
	public static bool Render(this Racr.AstNode n) { return n.AttValue<bool>("Render"); }
	public static string SExpr(this Racr.AstNode n) { return n.AttValue<string>("SExpr"); }

	// Rewriting
	public static void SetValue(this Racr.AstNode n, object value) { n.RewriteTerminal("value", value); }
}
