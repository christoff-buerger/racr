/* This file was generated with JastAdd2 (http://jastadd.org) version 2.1.8 */
package cjava.ast;

import java.util.regex.*;
import java.io.*;
/**
 * @ast node
 * @declaredat specifications/AST.ast:19
 * @production Statement : {@link ASTNode};

 */
public abstract class Statement extends ASTNode<ASTNode> implements Cloneable {
  /**
   * @aspect Unparser
   * @declaredat specifications/Unparser.jrag:11
   */
  public abstract void pp(Writer writer, int indent) throws IOException;
  /**
   * @aspect Unparser
   * @declaredat specifications/Unparser.jrag:13
   */
  public void printIdent(Writer writer, int indent) throws IOException {
		for (int i = 0; i < indent; i++)
			writer.write("\t");
	}
  /**
   * @declaredat ASTNode:1
   */
  public Statement() {
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
   * @apilevel low-level
   * @declaredat ASTNode:17
   */
  protected int numChildren() {
    
    
    state().addHandlerDepTo(numChildren_handler);
    return 0;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:26
   */
  public boolean mayHaveRewrite() {
    return false;
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:32
   */
  public void flushAttrCache() {
    super.flushAttrCache();
    isCorrect_reset();
    isCorrectLocal_reset();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:40
   */
  public void flushCollectionCache() {
    super.flushCollectionCache();
  }
  /**
   * @api internal
   * @declaredat ASTNode:46
   */
  public void flushRewriteCache() {
    super.flushRewriteCache();
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:52
   */
  public Statement clone() throws CloneNotSupportedException {
    Statement node = (Statement) super.clone();
    return node;
  }
  /**
   * Create a deep copy of the AST subtree at this node.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @deprecated Please use emitTreeCopy or emitTreeCopyNoTransform instead
   * @declaredat ASTNode:63
   */
  public abstract Statement fullCopy();
  /**
   * Create a deep copy of the AST subtree at this node.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @declaredat ASTNode:70
   */
  public abstract Statement treeCopyNoTransform();
  /**
   * Create a deep copy of the AST subtree at this node.
   * The subtree of this node is traversed to trigger rewrites before copy.
   * The copy is dangling, i.e. has no parent.
   * @return dangling copy of the subtree at this node
   * @apilevel low-level
   * @declaredat ASTNode:78
   */
  public abstract Statement treeCopy();
  /**
   * @apilevel internal
   * @declaredat ASTNode:82
   */
  protected boolean childIsNTA(int index) {
    return super.childIsNTA(index);
  }
  /**
   * @declaredat ASTNode:85
   */
  protected ASTNode$DepGraphNode isCorrect_handler;
  /**
   * @declaredat ASTNode:86
   */
  protected ASTNode$DepGraphNode isCorrectLocal_handler;
  /**
   * @declaredat ASTNode:87
   */
  protected void inc_copyHandlers(Statement copy) {
    super.inc_copyHandlers(copy);

        if (isCorrect_handler != null) {
          copy.isCorrect_handler = new ASTNode$DepGraphNode(isCorrect_handler, copy);
        }
        if (isCorrectLocal_handler != null) {
          copy.isCorrectLocal_handler = new ASTNode$DepGraphNode(isCorrectLocal_handler, copy);
        }
  }
  /**
   * @api internal
   * @declaredat ASTNode:102
   */
  public void reactToDependencyChange(String attrID) {
    
    // flush children
    
    
    
    
    
    if (attrID.equals("isCorrect") && isCorrect_computed) {
      isCorrect_computed = false;
      isCorrect_handler.notifyDependencies();
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
   * @declaredat ASTNode:134
   */
  protected void inc_flush_subtree(ASTNode$DepGraphNode h) {
    inc_state = inc_GARBAGE;
    h.transferSetsFrom(isCorrect_handler);
    
    h.transferSetsFrom(isCorrectLocal_handler);
    super.inc_flush_subtree(h);
  }
  /**
   * @apilevel internal
   * @declaredat ASTNode:147
   */
  public void inc_changeState(int newState) {
  super.inc_changeState(newState);
  if (isCorrect_handler != null) {
    isCorrect_handler.changeState(newState);
  }
  if (isCorrectLocal_handler != null) {
    isCorrectLocal_handler.changeState(newState);
  }
}
  /**
   * @apilevel internal
   * @declaredat ASTNode:159
   */
  public void inc_throwAway() {
  inc_state = inc_GARBAGE;
  super.inc_throwAway();
  if (isCorrect_handler != null) {
    isCorrect_handler.throwAway();
  }
  if (isCorrectLocal_handler != null) {
    isCorrectLocal_handler.throwAway();
  }
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
    isCorrect_value = isCorrectLocal();
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
    isCorrectLocal_value = true;
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
