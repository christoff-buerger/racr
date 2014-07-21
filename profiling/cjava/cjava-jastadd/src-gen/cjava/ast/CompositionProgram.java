/* This file was generated with JastAdd2 (http://jastadd.org) version 2.1.8 */
package cjava.ast;

import java.util.regex.*;
import java.io.*;
/**
 * @ast node
 * @declaredat specifications/AST.ast:12
 * @production CompositionProgram : {@link ASTNode} ::= <span class="component">Composers:{@link BindComposer}*</span>;

 */
public class CompositionProgram extends ASTNode<ASTNode> implements Cloneable {
  /**
   * @declaredat ASTNode:1
   */
  public CompositionProgram() {
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
    children = new ASTNode[1];
    state().enterConstruction();
    setChild(new List(), 0);
    state().exitConstruction();
  }
  /**
   * @declaredat ASTNode:16
   */
  public CompositionProgram(List<BindComposer> p0) {
state().enterConstruction();
    setChild(p0, 0);
state().exitConstruction();
  }
  /**
   * @apilevel low-level
   * @declaredat ASTNode:24
   */
  protected int numChildren() {
    
    
    state().addHandlerDepTo(numChildren_handler);
    return 1;
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
    nextComposition_reset();
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
  public CompositionProgram clone() throws CloneNotSupportedException {
    CompositionProgram node = (CompositionProgram) super.clone();
    return node;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:65
   */
  public CompositionProgram copy() {
    try {
      CompositionProgram node = (CompositionProgram) clone();
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
  public CompositionProgram fullCopy() {
    return treeCopyNoTransform();
  }
  /**
   * Create a deep copy of the AST subtree at this node.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @declaredat ASTNode:100
   */
  public CompositionProgram treeCopyNoTransform() {
    CompositionProgram tree = (CompositionProgram) copy();
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
  public CompositionProgram treeCopy() {
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
  protected ASTNode$DepGraphNode nextComposition_handler;
  /**
   * @declaredat ASTNode:138
   */
  protected void inc_copyHandlers(CompositionProgram copy) {
    super.inc_copyHandlers(copy);

        if (nextComposition_handler != null) {
          copy.nextComposition_handler = new ASTNode$DepGraphNode(nextComposition_handler, copy);
        }
  }
  /**
   * @api internal
   * @declaredat ASTNode:150
   */
  public void reactToDependencyChange(String attrID) {
    
    // flush children
    
    
    
    
    
    if (attrID.equals("nextComposition") && nextComposition_computed) {
      nextComposition_computed = false;
      nextComposition_value = null;
      nextComposition_handler.notifyDependencies();
      super.reactToDependencyChange(attrID);
      return;
    }
    super.reactToDependencyChange(attrID);
  }
  /**
   * @api internal
   * @declaredat ASTNode:176
   */
  protected void inc_flush_subtree(ASTNode$DepGraphNode h) {
    inc_state = inc_GARBAGE;
    h.transferSetsFrom(nextComposition_handler);
    super.inc_flush_subtree(h);
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:187
   */
  public void inc_changeState(int newState) {
  super.inc_changeState(newState);
  if (nextComposition_handler != null) {
    nextComposition_handler.changeState(newState);
  }
}
  /**
   * @apilevel internal
   * @declaredat ASTNode:196
   */
  public void inc_throwAway() {
  inc_state = inc_GARBAGE;
  super.inc_throwAway();
  if (nextComposition_handler != null) {
    nextComposition_handler.throwAway();
  }
}
  /**
   * Replaces the Composers list.
   * @param list The new list node to be used as the Composers list.
   * @apilevel high-level
   */
  public void setComposersList(List<BindComposer> list) {
    state().enterConstruction();
    setChild(list, 0);
    state().exitConstruction();
  }
  /**
   * Retrieves the number of children in the Composers list.
   * @return Number of children in the Composers list.
   * @apilevel high-level
   */
  public int getNumComposers() {
    return getComposersList().getNumChild();
  }
  /**
   * Retrieves the number of children in the Composers list.
   * Calling this method will not trigger rewrites.
   * @return Number of children in the Composers list.
   * @apilevel low-level
   */
  public int getNumComposersNoTransform() {
    return getComposersListNoTransform().getNumChildNoTransform();
  }
  /**
   * Retrieves the element at index {@code i} in the Composers list.
   * @param i Index of the element to return.
   * @return The element at position {@code i} in the Composers list.
   * @apilevel high-level
   */
  public BindComposer getComposers(int i) {
    return (BindComposer) getComposersList().getChild(i);
  }
  /**
   * Check whether the Composers list has any children.
   * @return {@code true} if it has at least one child, {@code false} otherwise.
   * @apilevel high-level
   */
  public boolean hasComposers() {
    return getComposersList().getNumChild() != 0;
  }
  /**
   * Append an element to the Composers list.
   * @param node The element to append to the Composers list.
   * @apilevel high-level
   */
  public void addComposers(BindComposer node) {
    List<BindComposer> list = (parent == null || state == null) ? getComposersListNoTransform() : getComposersList();
    list.addChild(node);
  }
  /**
   * @apilevel low-level
   */
  public void addComposersNoTransform(BindComposer node) {
    List<BindComposer> list = getComposersListNoTransform();
    list.addChild(node);
  }
  /**
   * Replaces the Composers list element at index {@code i} with the new node {@code node}.
   * @param node The new node to replace the old list element.
   * @param i The list index of the node to be replaced.
   * @apilevel high-level
   */
  public void setComposers(BindComposer node, int i) {
    List<BindComposer> list = getComposersList();
    state().enterConstruction();
    list.setChild(node, i);
    state().exitConstruction();
  }
  /**
   * Retrieves the Composers list.
   * @return The node representing the Composers list.
   * @apilevel high-level
   */
  @ASTNodeAnnotation.ListChild(name="Composers")
  public List<BindComposer> getComposersList() {
    List<BindComposer> list = (List<BindComposer>) getChild(0);
    list.getNumChild();
    return list;
  }
  /**
   * Retrieves the Composers list.
   * <p><em>This method does not invoke AST transformations.</em></p>
   * @return The node representing the Composers list.
   * @apilevel low-level
   */
  public List<BindComposer> getComposersListNoTransform() {
    return (List<BindComposer>) getChildNoTransform(0);
  }
  /**
   * Retrieves the Composers list.
   * @return The node representing the Composers list.
   * @apilevel high-level
   */
  public List<BindComposer> getComposerss() {
    return getComposersList();
  }
  /**
   * Retrieves the Composers list.
   * <p><em>This method does not invoke AST transformations.</em></p>
   * @return The node representing the Composers list.
   * @apilevel low-level
   */
  public List<BindComposer> getComposerssNoTransform() {
    return getComposersListNoTransform();
  }
  /**
   * @apilevel internal
   */
  protected boolean nextComposition_computed = false;
  /**
   * @apilevel internal
   */
  protected BindComposer nextComposition_value;
/**
 * @apilevel internal
 */
private void nextComposition_reset() {
  nextComposition_computed = false;
  nextComposition_value = null;
}  
  @ASTNodeAnnotation.Attribute
  public BindComposer nextComposition() {
    
    
    if (nextComposition_handler == null) {
      nextComposition_handler = new ASTNode$DepGraphNode(this, "nextComposition");
    }
    state().addHandlerDepTo(nextComposition_handler);
    
    
    
    
    if(nextComposition_computed) {
      return nextComposition_value;
    }
    
    
    ASTNode$DepGraphNode tmpHandler = new ASTNode$DepGraphNode(this, "");
    state().enterAttrStoreEval(tmpHandler);
    ASTNode$State state = state();
    boolean intermediate = state.INTERMEDIATE_VALUE;
    state.INTERMEDIATE_VALUE = false;
    int num = state.boundariesCrossed;
    boolean isFinal = this.is$Final();
    nextComposition_value = nextComposition_compute();
    if (isFinal && num == state().boundariesCrossed) {
      
      
      nextComposition_handler.transferDependenciesFrom(tmpHandler);
      nextComposition_computed = true;
    } else {
      
      
      tmpHandler.clearDependencies();
    }
    state.INTERMEDIATE_VALUE |= intermediate;

    
    
    state().exitAttrStoreEval(tmpHandler);
    
    
    
    
    return nextComposition_value;
  }
  /**
   * @apilevel internal
   */
  private BindComposer nextComposition_compute() {
  		for (int i = 0; i < getNumComposers(); i++) {
  			BindComposer next = getComposers(i);
  			if (!next.isExhausted())
  				return next;
  		}
  		return null;
  	}
  /**
   * @apilevel internal
   */
  public ASTNode rewriteTo() {    return super.rewriteTo();
  }}
