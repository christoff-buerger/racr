package cjava.ast;

import java.util.regex.*;
import java.io.*;
/**
 * @apilevel internal
 * @ast class
 * @declaredat ASTNode:4
 */
public class ASTNode$DepGraphNode extends java.lang.Object {



  // Level: attr

  public ASTNode fNode;


  public String fAttrID;



  public ASTNode$DepGraphNode(ASTNode node, String attrID) {
    fNode = node;
    fAttrID = attrID;
    fState = node.inc_state;
    //createdHandlers.add(this);
  }



  public ASTNode$DepGraphNode(ASTNode$DepGraphNode handler, ASTNode node) {
    fNode = node;
    fAttrID = handler.fAttrID;
    fState = node.inc_state;
    //createdHandlers.add(this);
  }



  public void flushRegion() {
    // Remove dependencies
    java.util.HashSet<ASTNode$DepGraphNode> k = fDependencySet;
    fDependencySet = new java.util.HashSet<ASTNode$DepGraphNode>(4);
    for (ASTNode$DepGraphNode node : k) {
      node.removeDependant(this);
    }
    fNode.reactToDependencyChange(fAttrID);
  }





  // Dependency management

  public java.util.HashSet<ASTNode$DepGraphNode> fListenerSet = new java.util.HashSet<ASTNode$DepGraphNode>(4);


  public java.util.HashSet<ASTNode$DepGraphNode> fDependencySet = new java.util.HashSet<ASTNode$DepGraphNode>(4);



  public boolean hasDependants() {
    return !fListenerSet.isEmpty();
  }



  public void addDependant(ASTNode$DepGraphNode node) {
    fListenerSet.add(node);
    node.addDependency(this);
  }



  public void removeDependant(ASTNode$DepGraphNode node) {
    fListenerSet.remove(node);
  }



  public void clearDependants() {
    for (ASTNode$DepGraphNode node : fListenerSet) {
      node.removeDependency(this);
    }
    fListenerSet.clear();
  }



  public void clearDependencies() {
    for (ASTNode$DepGraphNode node : fDependencySet) {
      node.removeDependant(this);
    }
    fDependencySet.clear();
  }



  public void addDependency(ASTNode$DepGraphNode node) {
    fDependencySet.add(node);
  }



  public void removeDependency(ASTNode$DepGraphNode node) {
    fDependencySet.remove(node);
  }



  /*
   * Transfers listeners from another handler, used in rewrites.
   */
  public void transferSetsFrom(ASTNode$DepGraphNode node) {
    if (node == null || this == node)
      return;
    for (ASTNode$DepGraphNode l : node.fListenerSet) {
      if (!l.isGarbage()) {
        this.addDependant(l);
      }
    }
    node.clearDependencies();
    node.clearDependants();
    node.throwAway();
  }



  /*
   * Transfers dependencies from another handler, used in rewrites.
   */
  public void transferDependenciesFrom(ASTNode$DepGraphNode node) {
    if (node == null || this == node)
      return;
    for (ASTNode$DepGraphNode l : node.fDependencySet) {
      l.addDependant(this);
    }
    node.clearDependencies();
  }



  // Notification

  private boolean visited = false;



  public void notifyDependencies() {
    // Notify and remove listeners
    if (!visited) {
      visited = true;
      java.util.HashSet<ASTNode$DepGraphNode> k = fListenerSet;
      fListenerSet = new java.util.HashSet<ASTNode$DepGraphNode>(4);
      for (ASTNode$DepGraphNode node : k) {
        if (!node.isGarbage()) {
          node.dependencyChanged();
        }
        node.removeDependency(this);
      }
      visited = false;
    }
  }



  // React to change

  private boolean visitedChange = false;



  public void dependencyChanged() {
    if (!visitedChange) {
      visitedChange = true;
      if (!fDependencySet.isEmpty()) {
        // Remove dependencies
        java.util.HashSet<ASTNode$DepGraphNode> k = fDependencySet;
        fDependencySet = new java.util.HashSet<ASTNode$DepGraphNode>(4);
        for (ASTNode$DepGraphNode node : k) {
          node.removeDependant(this);
        }
        fNode.reactToDependencyChange(fAttrID);
	  }
      visitedChange = false;
    }
  }



  // State

  protected int fState = ASTNode.inc_CREATED;



  public void changeState(int newState) {
    fState = newState;
  }



  public void throwAway() {
    fState = ASTNode.inc_GARBAGE;
  }



  public void keepAlive() {
    fState = ASTNode.inc_LIVE;
  }



  public boolean isGarbage() {
    return fState == ASTNode.inc_GARBAGE;
  }



  public boolean isCreated() {
    return fState == ASTNode.inc_CREATED;
  }



  public boolean isCloned() {
    return fState == ASTNode.inc_CLONED;
  }



  public boolean isLive() {
    return fState == ASTNode.inc_LIVE;
  }




  // Clean up

  public boolean visitedDuringCleanup = false;


  public static int nbr_cleanup = 0;



  public void cleanUpGarbage() {
    visitedDuringCleanup = true;
    nbr_cleanup++;
    // Clean up garbage
    java.util.Iterator<ASTNode$DepGraphNode> itr = fListenerSet.iterator();
    while (itr.hasNext()) {
      ASTNode$DepGraphNode cur = itr.next();
      if (cur.isGarbage()) {
        itr.remove();
      }
    }
  }


}
