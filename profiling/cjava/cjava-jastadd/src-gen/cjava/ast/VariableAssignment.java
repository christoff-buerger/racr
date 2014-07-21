/* This file was generated with JastAdd2 (http://jastadd.org) version 2.1.8 */
package cjava.ast;

import java.util.regex.*;
import java.io.*;
/**
 * @ast node
 * @declaredat specifications/AST.ast:36
 * @production VariableAssignment : {@link Statement} ::= <span class="component">LHand:{@link Reference}</span> <span class="component">RHand:{@link Reference}</span>;

 */
public class VariableAssignment extends Statement implements Cloneable {
  /**
   * @aspect Unparser
   * @declaredat specifications/Unparser.jrag:49
   */
  public void pp(Writer writer, int indent) throws IOException {
		printIdent(writer, indent);
		getLHand().pp(writer, 0);
		writer.write(" = ");
		getRHand().pp(writer, 0);
	}
  /**
   * @declaredat ASTNode:1
   */
  public VariableAssignment() {
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
    state().exitConstruction();
  }
  /**
   * @declaredat ASTNode:15
   */
  public VariableAssignment(Reference p0, Reference p1) {
state().enterConstruction();
    setChild(p0, 0);
    setChild(p1, 1);
state().exitConstruction();
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:24
   */
  protected int numChildren() {
    
    
    state().addHandlerDepTo(numChildren_handler);
    return 2;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:33
   */
  public boolean mayHaveRewrite() {
    return false;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:39
   */
  public void flushAttrCache() {
    super.flushAttrCache();
    isCorrect_reset();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:46
   */
  public void flushCollectionCache() {
    super.flushCollectionCache();
  }
  /**
   * @api internal
   * @declaredat ASTNode:52
   */
  public void flushRewriteCache() {
    super.flushRewriteCache();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:58
   */
  public VariableAssignment clone() throws CloneNotSupportedException {
    VariableAssignment node = (VariableAssignment) super.clone();
    return node;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:65
   */
  public VariableAssignment copy() {
    try {
      VariableAssignment node = (VariableAssignment) clone();
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
   * @declaredat ASTNode:91
   */
  public VariableAssignment fullCopy() {
    return treeCopyNoTransform();
  }
  /**
   * Create a deep copy of the AST subtree at this node.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @declaredat ASTNode:100
   */
  public VariableAssignment treeCopyNoTransform() {
    VariableAssignment tree = (VariableAssignment) copy();
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
   * @declaredat ASTNode:121
   */
  public VariableAssignment treeCopy() {
    doFullTraversal();
    return treeCopyNoTransform();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:128
   */
  protected boolean is$Equal(ASTNode node) {
    return super.is$Equal(node) ;    
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:134
   */
  protected boolean childIsNTA(int index) {
    return super.childIsNTA(index);
  }
  /**
   * @declaredat ASTNode:137
   */
  protected void inc_copyHandlers(VariableAssignment copy) {
    super.inc_copyHandlers(copy);

  }
  /**
   * @api internal
   * @declaredat ASTNode:146
   */
  public void reactToDependencyChange(String attrID) {
    
    // flush children
    
    
    
    
    
    if (attrID.equals("isCorrect") && isCorrect_computed) {
      isCorrect_computed = false;
      isCorrect_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    super.reactToDependencyChange(attrID);
  }
  /**
   * @api internal
   * @declaredat ASTNode:171
   */
  protected void inc_flush_subtree(ASTNode$DepGraphNode h) {
    inc_state = inc_GARBAGE;
    h.transferSetsFrom(isCorrect_handler);
    super.inc_flush_subtree(h);
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:182
   */
  public void inc_changeState(int newState) {
  super.inc_changeState(newState);
  if (isCorrect_handler != null) {
    isCorrect_handler.changeState(newState);
  }
}
  /**
   * @apilevel internal
   * @declaredat ASTNode:191
   */
  public void inc_throwAway() {
  inc_state = inc_GARBAGE;
  super.inc_throwAway();
  if (isCorrect_handler != null) {
    isCorrect_handler.throwAway();
  }
}
  /**
   * Replaces the LHand child.
   * @param node The new node to replace the LHand child.
   * @apilevel high-level
   */
  public void setLHand(Reference node) {
    state().enterConstruction();
    setChild(node, 0);
    state().exitConstruction();
  }
  /**
   * Retrieves the LHand child.
   * @return The current node used as the LHand child.
   * @apilevel high-level
   */
  @ASTNodeAnnotation.Child(name="LHand")
  public Reference getLHand() {
    return (Reference) getChild(0);
  }
  /**
   * Retrieves the LHand child.
   * <p><em>This method does not invoke AST transformations.</em></p>
   * @return The current node used as the LHand child.
   * @apilevel low-level
   */
  public Reference getLHandNoTransform() {
    return (Reference) getChildNoTransform(0);
  }
  /**
   * Replaces the RHand child.
   * @param node The new node to replace the RHand child.
   * @apilevel high-level
   */
  public void setRHand(Reference node) {
    state().enterConstruction();
    setChild(node, 1);
    state().exitConstruction();
  }
  /**
   * Retrieves the RHand child.
   * @return The current node used as the RHand child.
   * @apilevel high-level
   */
  @ASTNodeAnnotation.Child(name="RHand")
  public Reference getRHand() {
    return (Reference) getChild(1);
  }
  /**
   * Retrieves the RHand child.
   * <p><em>This method does not invoke AST transformations.</em></p>
   * @return The current node used as the RHand child.
   * @apilevel low-level
   */
  public Reference getRHandNoTransform() {
    return (Reference) getChildNoTransform(1);
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
    isCorrect_value = getLHand().isCorrect() && getRHand().isCorrect();
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
  public ASTNode rewriteTo() {    return super.rewriteTo();
  }}
