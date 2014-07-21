/* This file was generated with JastAdd2 (http://jastadd.org) version 2.1.8 */
package cjava.ast;

import java.util.regex.*;
import java.io.*;
/**
 * @ast node
 * @production Opt : {@link ASTNode};

 */
public class Opt<T extends ASTNode> extends ASTNode<T> implements Cloneable {
  /**
   * @declaredat ASTNode:1
   */
  public Opt() {
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
  public Opt(T opt) {
    state().enterConstruction();
    setChild(opt, 0);
    state().exitConstruction();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:22
   */
  public boolean mayHaveRewrite() {
    return false;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:28
   */
  public void flushAttrCache() {
    super.flushAttrCache();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:34
   */
  public void flushCollectionCache() {
    super.flushCollectionCache();
  }
  /**
   * @api internal
   * @declaredat ASTNode:40
   */
  public void flushRewriteCache() {
    super.flushRewriteCache();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:46
   */
  public Opt<T> clone() throws CloneNotSupportedException {
    Opt node = (Opt) super.clone();
    return node;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:53
   */
  public Opt<T> copy() {
    try {
      Opt node = (Opt) clone();
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
   * @declaredat ASTNode:79
   */
  public Opt<T> fullCopy() {
    return treeCopyNoTransform();
  }
  /**
   * Create a deep copy of the AST subtree at this node.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @declaredat ASTNode:88
   */
  public Opt<T> treeCopyNoTransform() {
    Opt tree = (Opt) copy();
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
   * @declaredat ASTNode:109
   */
  public Opt<T> treeCopy() {
    doFullTraversal();
    return treeCopyNoTransform();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:116
   */
  protected boolean is$Equal(ASTNode node) {
    return super.is$Equal(node) ;    
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:122
   */
  protected boolean childIsNTA(int index) {
    return super.childIsNTA(index);
  }
  /**
   * @declaredat ASTNode:125
   */
  protected void inc_copyHandlers(Opt copy) {
    super.inc_copyHandlers(copy);

  }
  /**
   * @api internal
   * @declaredat ASTNode:134
   */
  public void reactToDependencyChange(String attrID) {
    
    // flush children
    super.reactToDependencyChange(attrID);
  }
  /**
   * @api internal
   * @declaredat ASTNode:148
   */
  protected void inc_flush_subtree(ASTNode$DepGraphNode h) {
    inc_state = inc_GARBAGE;
    super.inc_flush_subtree(h);
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:158
   */
  public void inc_changeState(int newState) {
  super.inc_changeState(newState);
}
  /**
   * @apilevel internal
   * @declaredat ASTNode:164
   */
  public void inc_throwAway() {
  inc_state = inc_GARBAGE;
  super.inc_throwAway();
}
  /**
   * @apilevel internal
   */
  public ASTNode rewriteTo() {    return super.rewriteTo();
  }}
