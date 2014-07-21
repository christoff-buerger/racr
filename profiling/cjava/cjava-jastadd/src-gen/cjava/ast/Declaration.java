/* This file was generated with JastAdd2 (http://jastadd.org) version 2.1.8 */
package cjava.ast;

import java.util.regex.*;
import java.io.*;
/**
 * @ast node
 * @declaredat specifications/AST.ast:21
 * @production Declaration : {@link Statement} ::= <span class="component">&lt;Name:String&gt;</span>;

 */
public abstract class Declaration extends Statement implements Cloneable {
  /**
   * @declaredat ASTNode:1
   */
  public Declaration() {
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
    state().enterConstruction();
    state().exitConstruction();
  }
  /**
   * @declaredat ASTNode:14
   */
  public Declaration(String p0) {
state().enterConstruction();
    setName(p0);
state().exitConstruction();
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:22
   */
  protected int numChildren() {
    
    
    state().addHandlerDepTo(numChildren_handler);
    return 0;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:31
   */
  public boolean mayHaveRewrite() {
    return false;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:37
   */
  public void flushAttrCache() {
    super.flushAttrCache();
    lookup_String_reset();
    isCorrectLocal_reset();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:45
   */
  public void flushCollectionCache() {
    super.flushCollectionCache();
  }
  /**
   * @api internal
   * @declaredat ASTNode:51
   */
  public void flushRewriteCache() {
    super.flushRewriteCache();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:57
   */
  public Declaration clone() throws CloneNotSupportedException {
    Declaration node = (Declaration) super.clone();
    return node;
  }
  /**
   * Create a deep copy of the AST subtree at this node.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @deprecated Please use emitTreeCopy or emitTreeCopyNoTransform instead
   * @declaredat ASTNode:68
   */
  public abstract Declaration fullCopy();
  /**
   * Create a deep copy of the AST subtree at this node.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @declaredat ASTNode:75
   */
  public abstract Declaration treeCopyNoTransform();
  /**
   * Create a deep copy of the AST subtree at this node.
   * The subtree of this node is traversed to trigger rewrites before copy.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @declaredat ASTNode:83
   */
  public abstract Declaration treeCopy();
  /**
   * @apilevel internal
   * @declaredat ASTNode:87
   */
  protected boolean childIsNTA(int index) {
    return super.childIsNTA(index);
  }
  /**
   * @declaredat ASTNode:90
   */
  protected ASTNode$DepGraphNode lookup_String_handler;
  /**
   * @declaredat ASTNode:91
   */
  protected void inc_copyHandlers(Declaration copy) {
    super.inc_copyHandlers(copy);

        if (getName_handler != null) {
          copy.getName_handler = new ASTNode$DepGraphNode(getName_handler, copy);
        }
        if (lookup_String_handler != null) {
          copy.lookup_String_handler = new ASTNode$DepGraphNode(lookup_String_handler, copy);
        }
  }
  /**
   * @api internal
   * @declaredat ASTNode:106
   */
  public void reactToDependencyChange(String attrID) {
    
    // flush children
    
    
    
    
    
    if (attrID.equals("lookup_String") && lookup_String_values != null && !lookup_String_values.isEmpty()) {
      lookup_String_values = null;
      lookup_String_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    else 
    if (attrID.equals("isCorrectLocal") && isCorrectLocal_computed) {
      isCorrectLocal_computed = false;
      isCorrectLocal_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    super.reactToDependencyChange(attrID);
  }
  /**
   * @api internal
   * @declaredat ASTNode:138
   */
  protected void inc_flush_subtree(ASTNode$DepGraphNode h) {
    inc_state = inc_GARBAGE;
    h.transferSetsFrom(lookup_String_handler);
    
    h.transferSetsFrom(isCorrectLocal_handler);
    
    h.transferSetsFrom(getName_handler);
    super.inc_flush_subtree(h);
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:153
   */
  public void inc_changeState(int newState) {
  super.inc_changeState(newState);
  if (getName_handler != null) {
    getName_handler.changeState(newState);
  }
  if (lookup_String_handler != null) {
    lookup_String_handler.changeState(newState);
  }
  if (isCorrectLocal_handler != null) {
    isCorrectLocal_handler.changeState(newState);
  }
}
  /**
   * @apilevel internal
   * @declaredat ASTNode:168
   */
  public void inc_throwAway() {
  inc_state = inc_GARBAGE;
  super.inc_throwAway();
  if (getName_handler != null) {
    getName_handler.throwAway();
  }
  if (lookup_String_handler != null) {
    lookup_String_handler.throwAway();
  }
  if (isCorrectLocal_handler != null) {
    isCorrectLocal_handler.throwAway();
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
        ((Declaration)initial).setName(tokenString_Name);
        state().exitConstruction();
        return;
      }
    
    }
  }
  /**
   * @apilevel internal
   */
  protected String tokenString_Name;
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
  protected java.util.Map lookup_String_values;
/**
 * @apilevel internal
 */
private void lookup_String_reset() {
  lookup_String_values = null;
}  
  @ASTNodeAnnotation.Attribute
  public Declaration lookup(String label) {
    Object _parameters = label;
    if (lookup_String_values == null) lookup_String_values = new java.util.HashMap(4);
    
    
    if (lookup_String_handler == null) {
      lookup_String_handler = new ASTNode$DepGraphNode(this, "lookup_String");
    }
    state().addHandlerDepTo(lookup_String_handler);
    
    
    
    
    if(lookup_String_values.containsKey(_parameters)) {
      return (Declaration)lookup_String_values.get(_parameters);
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    Declaration lookup_String_value = null;
    if (isFinal && num == state().boundariesCrossed) {
      
      
      lookup_String_handler.transferDependenciesFrom(tmpHandler);
      lookup_String_values.put(_parameters, lookup_String_value);
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return lookup_String_value;
  }
  /**
   * @apilevel internal
   */
  protected boolean isCorrectLocal_computed = false;
  /**
   * @apilevel internal
   */
  protected boolean isCorrectLocal_value;
/**
 * @apilevel internal
 */
private void isCorrectLocal_reset() {
  isCorrectLocal_computed = false;
}  
  @ASTNodeAnnotation.Attribute
  public boolean isCorrectLocal() {
    
    
    if (isCorrectLocal_handler == null) {
      isCorrectLocal_handler = new ASTNode$DepGraphNode(this, "isCorrectLocal");
    }
    state().addHandlerDepTo(isCorrectLocal_handler);
    
    
    
    
    if(isCorrectLocal_computed) {
      return isCorrectLocal_value;
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    isCorrectLocal_value = this == getParent().getParent().localLookup(getName());
    if (isFinal && num == state().boundariesCrossed) {
      
      
      isCorrectLocal_handler.transferDependenciesFrom(tmpHandler);
      isCorrectLocal_computed = true;
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return isCorrectLocal_value;
  }
  /**
   * @apilevel internal
   */
  public ASTNode rewriteTo() {    return super.rewriteTo();
  }}
