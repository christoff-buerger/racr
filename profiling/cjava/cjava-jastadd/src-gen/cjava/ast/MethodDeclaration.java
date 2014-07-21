/* This file was generated with JastAdd2 (http://jastadd.org) version 2.1.8 */
package cjava.ast;

import java.util.regex.*;
import java.io.*;
/**
 * @ast node
 * @declaredat specifications/AST.ast:28
 * @production MethodDeclaration : {@link Declaration} ::= <span class="component">Parameters:{@link FieldDeclaration}*</span> <span class="component">Body:{@link Statement}*</span>;

 */
public class MethodDeclaration extends Declaration implements Cloneable {
  /**
   * @aspect Unparser
   * @declaredat specifications/Unparser.jrag:30
   */
  public void pp(Writer writer, int indent) throws IOException {
		printIdent(writer, indent);
		writer.write("public static void " + getName() + "(");
		if (hasParameters()) {
			getParameters(0).pp(writer, 0);
			for (int i = 1; i<getNumParameters(); i++) {
				writer.write(", ");
				getParameters(i).pp(writer, 0);
			}
		}
		writer.write(") {\n");
		for (Statement d:getBodyList()) {
			d.pp(writer, indent + 1);
			writer.write(";\n");
		}
		printIdent(writer, indent);
		writer.write("}\n");
	}
  /**
   * @declaredat ASTNode:1
   */
  public MethodDeclaration() {
    super();
  }
  /**
   * Initializes the child array to the correct size.
   * Initializes List and Opt nta children.
   * @apilevel internal
   * @ast method
   * @declaredat ASTNode:10
   */
  public void init$Children() {
    children = new ASTNode[2];
    state().enterConstruction();
    setChild(new List(), 0);
    setChild(new List(), 1);
    state().exitConstruction();
  }
  /**
   * @declaredat ASTNode:17
   */
  public MethodDeclaration(String p0, List<FieldDeclaration> p1, List<Statement> p2) {
state().enterConstruction();
    setName(p0);
    setChild(p1, 0);
    setChild(p2, 1);
state().exitConstruction();
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:27
   */
  protected int numChildren() {
    
    
    state().addHandlerDepTo(numChildren_handler);
    return 2;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:36
   */
  public boolean mayHaveRewrite() {
    return false;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:42
   */
  public void flushAttrCache() {
    super.flushAttrCache();
    localLookup_String_reset();
    localLookup_String_int_reset();
    isCorrect_reset();
    lookupRef_String_reset();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:52
   */
  public void flushCollectionCache() {
    super.flushCollectionCache();
  }
  /**
   * @api internal
   * @declaredat ASTNode:58
   */
  public void flushRewriteCache() {
    super.flushRewriteCache();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:64
   */
  public MethodDeclaration clone() throws CloneNotSupportedException {
    MethodDeclaration node = (MethodDeclaration) super.clone();
    return node;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:71
   */
  public MethodDeclaration copy() {
    try {
      MethodDeclaration node = (MethodDeclaration) clone();
      node.parent = null;
      if(children != null) {
        node.children = (ASTNode[]) children.clone();
      }
      node.inc_state = inc_CLONED;
      for (int i = 0; node.children != null && i < node.children.length; i++) {
        node.children[i] = null;
      }
      node.init_children = null;
      node.children_computed = null;
      inc_copyHandlers(node);
      return node;
    } catch (CloneNotSupportedException e) {
      throw new Error("Error: clone not supported for " + getClass().getName());
    }
  }
  /**
   * Create a deep copy of the AST subtree at this node.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @deprecated Please use emitTreeCopy or emitTreeCopyNoTransform instead
   * @declaredat ASTNode:97
   */
  public MethodDeclaration fullCopy() {
    return treeCopyNoTransform();
  }
  /**
   * Create a deep copy of the AST subtree at this node.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @declaredat ASTNode:106
   */
  public MethodDeclaration treeCopyNoTransform() {
    MethodDeclaration tree = (MethodDeclaration) copy();
    if (children != null) {
      for (int i = 0; i < children.length; ++i) {
        ASTNode child = (ASTNode) children[i];
        if(child != null) {
          child = child.treeCopyNoTransform();
          tree.children[i] = child;
          child.parent = tree;
        }
      }
    }
    return tree;
  }
  /**
   * Create a deep copy of the AST subtree at this node.
   * The subtree of this node is traversed to trigger rewrites before copy.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @declaredat ASTNode:127
   */
  public MethodDeclaration treeCopy() {
    doFullTraversal();
    return treeCopyNoTransform();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:134
   */
  protected boolean is$Equal(ASTNode node) {
    return super.is$Equal(node)  && (tokenString_Name == ((MethodDeclaration)node).tokenString_Name);    
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:140
   */
  protected boolean childIsNTA(int index) {
    return super.childIsNTA(index);
  }
  /**
   * @declaredat ASTNode:143
   */
  protected ASTNode$DepGraphNode localLookup_String_handler;
  /**
   * @declaredat ASTNode:144
   */
  protected ASTNode$DepGraphNode localLookup_String_int_handler;
  /**
   * @declaredat ASTNode:145
   */
  protected ASTNode$DepGraphNode lookupRef_String_handler;
  /**
   * @declaredat ASTNode:146
   */
  protected void inc_copyHandlers(MethodDeclaration copy) {
    super.inc_copyHandlers(copy);

        if (localLookup_String_handler != null) {
          copy.localLookup_String_handler = new ASTNode$DepGraphNode(localLookup_String_handler, copy);
        }
        if (localLookup_String_int_handler != null) {
          copy.localLookup_String_int_handler = new ASTNode$DepGraphNode(localLookup_String_int_handler, copy);
        }
        if (lookupRef_String_handler != null) {
          copy.lookupRef_String_handler = new ASTNode$DepGraphNode(lookupRef_String_handler, copy);
        }
  }
  /**
   * @api internal
   * @declaredat ASTNode:164
   */
  public void reactToDependencyChange(String attrID) {
    
    // flush children
    
    
    
    
    
    if (attrID.equals("localLookup_String") && localLookup_String_values != null && !localLookup_String_values.isEmpty()) {
      localLookup_String_values = null;
      localLookup_String_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    else 
    if (attrID.equals("localLookup_String_int") && localLookup_String_int_values != null && !localLookup_String_int_values.isEmpty()) {
      localLookup_String_int_values = null;
      localLookup_String_int_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    else 
    if (attrID.equals("isCorrect") && isCorrect_computed) {
      isCorrect_computed = false;
      isCorrect_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    else 
    if (attrID.equals("lookupRef_String") && lookupRef_String_values != null && !lookupRef_String_values.isEmpty()) {
      lookupRef_String_values = null;
      lookupRef_String_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    super.reactToDependencyChange(attrID);
  }
  /**
   * @api internal
   * @declaredat ASTNode:210
   */
  protected void inc_flush_subtree(ASTNode$DepGraphNode h) {
    inc_state = inc_GARBAGE;
    h.transferSetsFrom(localLookup_String_handler);
    
    h.transferSetsFrom(localLookup_String_int_handler);
    
    h.transferSetsFrom(isCorrect_handler);
    
    h.transferSetsFrom(lookupRef_String_handler);
    super.inc_flush_subtree(h);
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:227
   */
  public void inc_changeState(int newState) {
  super.inc_changeState(newState);
  if (localLookup_String_handler != null) {
    localLookup_String_handler.changeState(newState);
  }
  if (localLookup_String_int_handler != null) {
    localLookup_String_int_handler.changeState(newState);
  }
  if (isCorrect_handler != null) {
    isCorrect_handler.changeState(newState);
  }
  if (lookupRef_String_handler != null) {
    lookupRef_String_handler.changeState(newState);
  }
}
  /**
   * @apilevel internal
   * @declaredat ASTNode:245
   */
  public void inc_throwAway() {
  inc_state = inc_GARBAGE;
  super.inc_throwAway();
  if (localLookup_String_handler != null) {
    localLookup_String_handler.throwAway();
  }
  if (localLookup_String_int_handler != null) {
    localLookup_String_int_handler.throwAway();
  }
  if (isCorrect_handler != null) {
    isCorrect_handler.throwAway();
  }
  if (lookupRef_String_handler != null) {
    lookupRef_String_handler.throwAway();
  }
}
  /**
   */
  protected ASTNode$DepGraphNode getName_handler = new ASTNode$DepGraphNode(this, "getName");
  /**
   * Replaces the lexeme Name.
   * @param value The new value for the lexeme Name.
   * @apilevel high-level
   */
  public void setName(String value) {
    tokenString_Name = value;
    
    
    if (!state().IN_CONSTRUCTION && !state().IN_ATTR_STORE_EVAL) {
      getName_handler.notifyDependencies();
    
    
    
      ASTNode initial = inc_locateInitialCopy();
      if (initial != null) {
        state().enterConstruction();
        ((MethodDeclaration)initial).setName(tokenString_Name);
        state().exitConstruction();
        return;
      }
    
    }
  }
  /**
   * Retrieves the value for the lexeme Name.
   * @return The value for the lexeme Name.
   * @apilevel high-level
   */
  @ASTNodeAnnotation.Token(name="Name")
  public String getName() {
    
    
    state().addHandlerDepTo(getName_handler);
    return tokenString_Name != null ? tokenString_Name : "";
  }
  /**
   * Replaces the Parameters list.
   * @param list The new list node to be used as the Parameters list.
   * @apilevel high-level
   */
  public void setParametersList(List<FieldDeclaration> list) {
    state().enterConstruction();
    setChild(list, 0);
    state().exitConstruction();
  }
  /**
   * Retrieves the number of children in the Parameters list.
   * @return Number of children in the Parameters list.
   * @apilevel high-level
   */
  public int getNumParameters() {
    return getParametersList().getNumChild();
  }
  /**
   * Retrieves the number of children in the Parameters list.
   * Calling this method will not trigger rewrites.
   * @return Number of children in the Parameters list.
   * @apilevel low-level
   */
  public int getNumParametersNoTransform() {
    return getParametersListNoTransform().getNumChildNoTransform();
  }
  /**
   * Retrieves the element at index {@code i} in the Parameters list.
   * @param i Index of the element to return.
   * @return The element at position {@code i} in the Parameters list.
   * @apilevel high-level
   */
  public FieldDeclaration getParameters(int i) {
    return (FieldDeclaration) getParametersList().getChild(i);
  }
  /**
   * Check whether the Parameters list has any children.
   * @return {@code true} if it has at least one child, {@code false} otherwise.
   * @apilevel high-level
   */
  public boolean hasParameters() {
    return getParametersList().getNumChild() != 0;
  }
  /**
   * Append an element to the Parameters list.
   * @param node The element to append to the Parameters list.
   * @apilevel high-level
   */
  public void addParameters(FieldDeclaration node) {
    List<FieldDeclaration> list = (parent == null || state == null) ? getParametersListNoTransform() : getParametersList();
    list.addChild(node);
  }
  /**
   * @apilevel low-level
   */
  public void addParametersNoTransform(FieldDeclaration node) {
    List<FieldDeclaration> list = getParametersListNoTransform();
    list.addChild(node);
  }
  /**
   * Replaces the Parameters list element at index {@code i} with the new node {@code node}.
   * @param node The new node to replace the old list element.
   * @param i The list index of the node to be replaced.
   * @apilevel high-level
   */
  public void setParameters(FieldDeclaration node, int i) {
    List<FieldDeclaration> list = getParametersList();
    state().enterConstruction();
    list.setChild(node, i);
    state().exitConstruction();
  }
  /**
   * Retrieves the Parameters list.
   * @return The node representing the Parameters list.
   * @apilevel high-level
   */
  @ASTNodeAnnotation.ListChild(name="Parameters")
  public List<FieldDeclaration> getParametersList() {
    List<FieldDeclaration> list = (List<FieldDeclaration>) getChild(0);
    list.getNumChild();
    return list;
  }
  /**
   * Retrieves the Parameters list.
   * <p><em>This method does not invoke AST transformations.</em></p>
   * @return The node representing the Parameters list.
   * @apilevel low-level
   */
  public List<FieldDeclaration> getParametersListNoTransform() {
    return (List<FieldDeclaration>) getChildNoTransform(0);
  }
  /**
   * Retrieves the Parameters list.
   * @return The node representing the Parameters list.
   * @apilevel high-level
   */
  public List<FieldDeclaration> getParameterss() {
    return getParametersList();
  }
  /**
   * Retrieves the Parameters list.
   * <p><em>This method does not invoke AST transformations.</em></p>
   * @return The node representing the Parameters list.
   * @apilevel low-level
   */
  public List<FieldDeclaration> getParameterssNoTransform() {
    return getParametersListNoTransform();
  }
  /**
   * Replaces the Body list.
   * @param list The new list node to be used as the Body list.
   * @apilevel high-level
   */
  public void setBodyList(List<Statement> list) {
    state().enterConstruction();
    setChild(list, 1);
    state().exitConstruction();
  }
  /**
   * Retrieves the number of children in the Body list.
   * @return Number of children in the Body list.
   * @apilevel high-level
   */
  public int getNumBody() {
    return getBodyList().getNumChild();
  }
  /**
   * Retrieves the number of children in the Body list.
   * Calling this method will not trigger rewrites.
   * @return Number of children in the Body list.
   * @apilevel low-level
   */
  public int getNumBodyNoTransform() {
    return getBodyListNoTransform().getNumChildNoTransform();
  }
  /**
   * Retrieves the element at index {@code i} in the Body list.
   * @param i Index of the element to return.
   * @return The element at position {@code i} in the Body list.
   * @apilevel high-level
   */
  public Statement getBody(int i) {
    return (Statement) getBodyList().getChild(i);
  }
  /**
   * Check whether the Body list has any children.
   * @return {@code true} if it has at least one child, {@code false} otherwise.
   * @apilevel high-level
   */
  public boolean hasBody() {
    return getBodyList().getNumChild() != 0;
  }
  /**
   * Append an element to the Body list.
   * @param node The element to append to the Body list.
   * @apilevel high-level
   */
  public void addBody(Statement node) {
    List<Statement> list = (parent == null || state == null) ? getBodyListNoTransform() : getBodyList();
    list.addChild(node);
  }
  /**
   * @apilevel low-level
   */
  public void addBodyNoTransform(Statement node) {
    List<Statement> list = getBodyListNoTransform();
    list.addChild(node);
  }
  /**
   * Replaces the Body list element at index {@code i} with the new node {@code node}.
   * @param node The new node to replace the old list element.
   * @param i The list index of the node to be replaced.
   * @apilevel high-level
   */
  public void setBody(Statement node, int i) {
    List<Statement> list = getBodyList();
    state().enterConstruction();
    list.setChild(node, i);
    state().exitConstruction();
  }
  /**
   * Retrieves the Body list.
   * @return The node representing the Body list.
   * @apilevel high-level
   */
  @ASTNodeAnnotation.ListChild(name="Body")
  public List<Statement> getBodyList() {
    List<Statement> list = (List<Statement>) getChild(1);
    list.getNumChild();
    return list;
  }
  /**
   * Retrieves the Body list.
   * <p><em>This method does not invoke AST transformations.</em></p>
   * @return The node representing the Body list.
   * @apilevel low-level
   */
  public List<Statement> getBodyListNoTransform() {
    return (List<Statement>) getChildNoTransform(1);
  }
  /**
   * Retrieves the Body list.
   * @return The node representing the Body list.
   * @apilevel high-level
   */
  public List<Statement> getBodys() {
    return getBodyList();
  }
  /**
   * Retrieves the Body list.
   * <p><em>This method does not invoke AST transformations.</em></p>
   * @return The node representing the Body list.
   * @apilevel low-level
   */
  public List<Statement> getBodysNoTransform() {
    return getBodyListNoTransform();
  }
  protected java.util.Map localLookup_String_values;
/**
 * @apilevel internal
 */
private void localLookup_String_reset() {
  localLookup_String_values = null;
}  
  @ASTNodeAnnotation.Attribute
  public Declaration localLookup(String label) {
    Object _parameters = label;
    if (localLookup_String_values == null) localLookup_String_values = new java.util.HashMap(4);
    
    
    if (localLookup_String_handler == null) {
      localLookup_String_handler = new ASTNode$DepGraphNode(this, "localLookup_String");
    }
    state().addHandlerDepTo(localLookup_String_handler);
    
    
    
    
    if(localLookup_String_values.containsKey(_parameters)) {
      return (Declaration)localLookup_String_values.get(_parameters);
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    Declaration localLookup_String_value = localLookup_compute(label);
    if (isFinal && num == state().boundariesCrossed) {
      
      
      localLookup_String_handler.transferDependenciesFrom(tmpHandler);
      localLookup_String_values.put(_parameters, localLookup_String_value);
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return localLookup_String_value;
  }
  /**
   * @apilevel internal
   */
  private Declaration localLookup_compute(String label) {
  	   for (Declaration d : getParametersList())
  		   if (label.matches("-?[0-9]+") || label.equals("*") || label.equals(d.getName()))
  			   return d;
  	   for (Statement d : getBodyList()){
  		   if (d instanceof Declaration)
  			   if (label.matches("-?[0-9]+") || label.equals("*") || label.equals(((Declaration)d).getName()))
  				   return (Declaration)d;
  	   }
  	   return null;
  	}
  protected java.util.Map localLookup_String_int_values;
/**
 * @apilevel internal
 */
private void localLookup_String_int_reset() {
  localLookup_String_int_values = null;
}  
  @ASTNodeAnnotation.Attribute
  public Declaration localLookup(String label, int bound) {
    java.util.List _parameters = new java.util.ArrayList(2);
    _parameters.add(label);
    _parameters.add(Integer.valueOf(bound));
    if (localLookup_String_int_values == null) localLookup_String_int_values = new java.util.HashMap(4);
    
    
    if (localLookup_String_int_handler == null) {
      localLookup_String_int_handler = new ASTNode$DepGraphNode(this, "localLookup_String_int");
    }
    state().addHandlerDepTo(localLookup_String_int_handler);
    
    
    
    
    if(localLookup_String_int_values.containsKey(_parameters)) {
      return (Declaration)localLookup_String_int_values.get(_parameters);
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    Declaration localLookup_String_int_value = localLookup_compute(label, bound);
    if (isFinal && num == state().boundariesCrossed) {
      
      
      localLookup_String_int_handler.transferDependenciesFrom(tmpHandler);
      localLookup_String_int_values.put(_parameters, localLookup_String_int_value);
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return localLookup_String_int_value;
  }
  /**
   * @apilevel internal
   */
  private Declaration localLookup_compute(String label, int bound) {
  	   for (Declaration d : getParametersList())
  		   if (label.matches("-?[0-9]+") || label.equals("*") || label.equals(d.getName()))
  			   return d;
  	   for (int i = 0; i < bound; i++) {
  		   Statement d = getBody(i);
  		   if (d instanceof Declaration)
  			   if (label.matches("-?[0-9]+") || label.equals("*") || label.equals(((Declaration)d).getName()))
  				   return (Declaration)d;
  	   }
  	   return null;
  	}
  /**
   * @apilevel internal
   */
  protected boolean isCorrect_computed = false;
  /**
   * @apilevel internal
   */
  protected boolean isCorrect_value;
/**
 * @apilevel internal
 */
private void isCorrect_reset() {
  isCorrect_computed = false;
}  
  @ASTNodeAnnotation.Attribute
  public boolean isCorrect() {
    
    
    if (isCorrect_handler == null) {
      isCorrect_handler = new ASTNode$DepGraphNode(this, "isCorrect");
    }
    state().addHandlerDepTo(isCorrect_handler);
    
    
    
    
    if(isCorrect_computed) {
      return isCorrect_value;
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    isCorrect_value = isCorrect_compute();
    if (isFinal && num == state().boundariesCrossed) {
      
      
      isCorrect_handler.transferDependenciesFrom(tmpHandler);
      isCorrect_computed = true;
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return isCorrect_value;
  }
  /**
   * @apilevel internal
   */
  private boolean isCorrect_compute() {
  		if (!isCorrectLocal())
  			return false;
  		for (Declaration d : getParametersList())
  			if (!d.isCorrect())
  				return false;
  		for (Statement d : getBodyList())
  			if (!d.isCorrect())
  				return false;
  		return true;
  	}
  /**
   * @attribute inh
   * @aspect NameAnalysis
   * @declaredat specifications/NameAnalysis.jrag:120
   */
  @ASTNodeAnnotation.Attribute
  public Declaration lookupRef(String refName) {
    Object _parameters = refName;
    if (lookupRef_String_values == null) lookupRef_String_values = new java.util.HashMap(4);
    
    
    if (lookupRef_String_handler == null) {
      lookupRef_String_handler = new ASTNode$DepGraphNode(this, "lookupRef_String");
    }
    state().addHandlerDepTo(lookupRef_String_handler);
    
    
    
    
    if(lookupRef_String_values.containsKey(_parameters)) {
      return (Declaration)lookupRef_String_values.get(_parameters);
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    Declaration lookupRef_String_value = getParent().Define_Declaration_lookupRef(this, null, refName);
    if (isFinal && num == state().boundariesCrossed) {
      
      
      lookupRef_String_handler.transferDependenciesFrom(tmpHandler);
      lookupRef_String_values.put(_parameters, lookupRef_String_value);
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return lookupRef_String_value;
  }
  protected java.util.Map lookupRef_String_values;
/**
 * @apilevel internal
 */
private void lookupRef_String_reset() {
  lookupRef_String_values = null;
}  
  /**
   * @declaredat specifications/NameAnalysis.jrag:133
   * @apilevel internal
   */
  public Declaration Define_Declaration_lookupRef(ASTNode caller, ASTNode child, String refName) {
    if (caller == getBodyListNoTransform()) {
      int i = caller.getIndexOfChild(child);
      {
		Declaration match = (refName.indexOf('.') == -1) ? localLookup(refName, i) : null;
		return (match != null) ? match : lookupRef(refName);
	}
    }
    else {
      return getParent().Define_Declaration_lookupRef(this, caller, refName);
    }
  }
  /**
   * @apilevel internal
   */
  public ASTNode rewriteTo() {    return super.rewriteTo();
  }}
