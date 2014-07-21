package cjava.ast;

import java.util.regex.*;
import java.io.*;
/**
 * @apilevel internal
 * @ast class
 * @declaredat ASTNode$State:4
 */
public class ASTNode$State extends java.lang.Object {

  /**
   * @apilevel internal
   */
  public boolean LAST_CYCLE = false;


    
  /**
   * @apilevel internal
   */
  public boolean INTERMEDIATE_VALUE = false;

 
  
  /**
   * @apilevel internal
   */
  public boolean IN_CIRCLE = false;



  /**
   * @apilevel internal
   */
  public int CIRCLE_INDEX = 1;



  /**
   * @apilevel internal
   */
  public boolean CHANGE = false;



  /**
   * @apilevel internal
   */
  public boolean RESET_CYCLE = false;



  /**
   * @apilevel internal
   */
  static public class CircularValue {
    Object value;
    int visited = -1;
  }



  /**
   * @apilevel internal
   */
  public static final int REWRITE_CHANGE = 1;



  /**
   * @apilevel internal
   */
  public static final int REWRITE_NOCHANGE = 2;



  /**
   * @apilevel internal
   */
  public static final int REWRITE_INTERRUPT = 3;



  public int boundariesCrossed = 0;



  // state code
  private int[] stack;



  private int pos;



  public ASTNode$State() {
    stack = new int[64];
    pos = 0;
  }



  private void ensureSize(int size) {
    if(size < stack.length)
      return;
    int[] newStack = new int[stack.length * 2];
    System.arraycopy(stack, 0, newStack, 0, stack.length);
    stack = newStack;
  }



  public void push(int i) {
    ensureSize(pos+1);
    stack[pos++] = i;
  }



  public int pop() {
    return stack[--pos];
  }



  public int peek() {
    return stack[pos-1];
  }



  /**
   * @apilevel internal
   */
  static class IdentityHashSet extends java.util.AbstractSet implements java.util.Set {

    public IdentityHashSet(int initialCapacity) {
      map = new java.util.IdentityHashMap(initialCapacity);
    }

    private java.util.IdentityHashMap map;

    private static final Object PRESENT = new Object();

    public java.util.Iterator iterator() {
      return map.keySet().iterator();
    }

    public int size() {
      return map.size();
    }

    public boolean isEmpty() {
      return map.isEmpty();
    }

    public boolean contains(Object o) {
      return map.containsKey(o);
    }

    public boolean add(Object o) {
      return map.put(o, PRESENT)==null;
    }

    public boolean remove(Object o) {
      return map.remove(o)==PRESENT;
    }

    public void clear() {
      map.clear();
    }
  }


  
  
    protected java.util.Stack handlerAttrStack = new java.util.Stack();


  
  //protected java.util.Stack handlerRewriteStack = new java.util.Stack();
  
    public void addHandlerDepTo(ASTNode$DepGraphNode handler) {
      if (!IN_ATTR_STORE_EVAL || handler == null) {
  //  if (!IN_ATTR_STORE_EVAL && !IN_REWRITE_EVAL || handler == null) {
        return;
      }
      java.util.Stack handlerStack = handlerAttrStack;
  //  if (IN_REWRITE_EVAL)
  //    handlerStack = handlerRewriteStack;
      if (!handlerStack.isEmpty()) {
        //throw new Error("Handler stack is empty at addition of dependency!");
        ASTNode$DepGraphNode top = (ASTNode$DepGraphNode) handlerStack.peek();
        handler.addDependant(top);
  
      }
    }


  
    public boolean IN_ATTR_STORE_EVAL = false;


  
    public void enterAttrStoreEval(ASTNode$DepGraphNode handler) {
  //  if (!IN_REWRITE_EVAL) {
      IN_ATTR_STORE_EVAL = true;
      //System.out.println("attr eval stack enter: " + handler.fAttrID);
      pushHandler(handlerAttrStack, handler);
  //  }
    }


  
    public void exitAttrStoreEval(ASTNode$DepGraphNode handler) {
  //  if (!IN_REWRITE_EVAL) {
      popHandler(handlerAttrStack, handler);
      //System.out.println("attr eval stack exit: " + handler.fAttrID);
      IN_ATTR_STORE_EVAL = !handlerAttrStack.isEmpty();
  //  }
    }


  
  //public boolean IN_REWRITE_EVAL = false;
  
    public void enterRewriteEval(ASTNode$DepGraphNode handler) {
      enterAttrStoreEval(handler);
  //  if (grammar().incrementalTrack) {
  //    ASTNode$DepGraphNode.trackedComp++;
  //  }
  //  IN_REWRITE_EVAL = true;
  //  pushHandler(handlerRewriteStack, handler);
    }


  
    public void exitRewriteEval(ASTNode$DepGraphNode handler) {
      exitAttrStoreEval(handler);
  //  if (IN_REWRITE_EVAL) {
  //    popHandler(handlerRewriteStack, handler);
  //    IN_REWRITE_EVAL = !handlerRewriteStack.isEmpty();
  //  }
    }


  
    public boolean IN_CONSTRUCTION = false;


  
    private int inc_constructionCount = 0;


  
    public void enterConstruction() {
      IN_CONSTRUCTION = true;
      inc_constructionCount++;
    }


  
    public void exitConstruction() {
      inc_constructionCount--;
      if (inc_constructionCount == 0)
      IN_CONSTRUCTION = false;
    }


  
    protected void pushHandler(java.util.Stack stack, ASTNode$DepGraphNode handler) {
      stack.push(handler);
    }


  
    protected ASTNode$DepGraphNode popHandler(java.util.Stack stack, ASTNode$DepGraphNode handler) {
      if (stack.isEmpty())
        throw new Error("Handler stack is empty at exit!");
      ASTNode$DepGraphNode h = (ASTNode$DepGraphNode)stack.pop();
  //  Skip this for now, deal with transitive dependencies later
  //  if (grammar().incrementalPropLimit) {
  //    h.setCacheInDependent(h.noCacheRead);
  //    if (!stack.isEmpty()) {
  //      ASTNode$DepGraphNode top = (ASTNode$DepGraphNode) stack.peek();
  //      top.noCacheRead &= h.noCacheRead;
  //    }
  //  }
      if (h != handler)
        throw new Error("Top of handler stack does not match at pop!");
  //    throw new Error("Top of handler stack does not match at pop! [" + handler.fAttrID + "]");
      return h;
    }

public void reset() {
    IN_CIRCLE = false;
    CIRCLE_INDEX = 1;
    CHANGE = false;
    LAST_CYCLE = false;

    boundariesCrossed = 0;

  }


}
