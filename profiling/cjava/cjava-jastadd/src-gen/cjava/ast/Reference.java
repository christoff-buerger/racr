/* This file was generated with JastAdd2 (http://jastadd.org) version 2.1.8 */
package cjava.ast;

import java.util.regex.*;
import java.io.*;
/**
 * @ast node
 * @declaredat specifications/AST.ast:40
 * @production Reference : {@link Statement} ::= <span class="component">&lt;Name:String&gt;</span>;

 */
public class Reference extends Statement implements Cloneable {
  /**
   * @aspect Unparser
   * @declaredat specifications/Unparser.jrag:69
   */
  public void pp(Writer writer, int indent) throws IOException {
		printIdent(writer, indent);
		writer.write(getName());
	}
  /**
   * @declaredat ASTNode:1
   */
  public Reference() {
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
  public Reference(String p0) {
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
    isCorrectLocal_reset();
    lookupRef_String_reset();
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
  public Reference clone() throws CloneNotSupportedException {
    Reference node = (Reference) super.clone();
    return node;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:64
   */
  public Reference copy() {
    try {
      Reference node = (Reference) clone();
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
   * @declaredat ASTNode:90
   */
  public Reference fullCopy() {
    return treeCopyNoTransform();
  }
  /**
   * Create a deep copy of the AST subtree at this node.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @declaredat ASTNode:99
   */
  public Reference treeCopyNoTransform() {
    Reference tree = (Reference) copy();
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
   * @declaredat ASTNode:120
   */
  public Reference treeCopy() {
    doFullTraversal();
    return treeCopyNoTransform();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:127
   */
  protected boolean is$Equal(ASTNode node) {
    return super.is$Equal(node)  && (tokenString_Name == ((Reference)node).tokenString_Name);    
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:133
   */
  protected boolean childIsNTA(int index) {
    return super.childIsNTA(index);
  }
  /**
   * @declaredat ASTNode:136
   */
  protected ASTNode$DepGraphNode lookupRef_String_handler;
  /**
   * @declaredat ASTNode:137
   */
  protected void inc_copyHandlers(Reference copy) {
    super.inc_copyHandlers(copy);

        if (getName_handler != null) {
          copy.getName_handler = new ASTNode$DepGraphNode(getName_handler, copy);
        }
        if (lookupRef_String_handler != null) {
          copy.lookupRef_String_handler = new ASTNode$DepGraphNode(lookupRef_String_handler, copy);
        }
  }
  /**
   * @api internal
   * @declaredat ASTNode:152
   */
  public void reactToDependencyChange(String attrID) {
    
    // flush children
    
    
    
    
    
    if (attrID.equals("isCorrectLocal") && isCorrectLocal_computed) {
      isCorrectLocal_computed = false;
      isCorrectLocal_handler.notifyDependencies();
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
   * @declaredat ASTNode:184
   */
  protected void inc_flush_subtree(ASTNode$DepGraphNode h) {
    inc_state = inc_GARBAGE;
    h.transferSetsFrom(isCorrectLocal_handler);
    
    h.transferSetsFrom(lookupRef_String_handler);
    
    h.transferSetsFrom(getName_handler);
    super.inc_flush_subtree(h);
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:199
   */
  public void inc_changeState(int newState) {
  super.inc_changeState(newState);
  if (getName_handler != null) {
    getName_handler.changeState(newState);
  }
  if (isCorrectLocal_handler != null) {
    isCorrectLocal_handler.changeState(newState);
  }
  if (lookupRef_String_handler != null) {
    lookupRef_String_handler.changeState(newState);
  }
}
  /**
   * @apilevel internal
   * @declaredat ASTNode:214
   */
  public void inc_throwAway() {
  inc_state = inc_GARBAGE;
  super.inc_throwAway();
  if (getName_handler != null) {
    getName_handler.throwAway();
  }
  if (isCorrectLocal_handler != null) {
    isCorrectLocal_handler.throwAway();
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
        ((Reference)initial).setName(tokenString_Name);
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
    isCorrectLocal_value = isCorrectLocal_compute();
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
  private boolean isCorrectLocal_compute() {
  		Declaration decl = lookupRef(getName());
  		return decl != null && decl instanceof FieldDeclaration;
  	}
  /**
   * @attribute inh
   * @aspect NameAnalysis
   * @declaredat specifications/NameAnalysis.jrag:121
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
   * @apilevel internal
   */
  public ASTNode rewriteTo() {    return super.rewriteTo();
  }}
