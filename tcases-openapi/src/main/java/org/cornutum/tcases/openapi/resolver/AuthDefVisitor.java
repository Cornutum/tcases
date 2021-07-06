//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

/**
 * Defines the Visitor pattern for {@link AuthDef} objects.
 */
public interface AuthDefVisitor
  {
  public void visit( ApiKeyDef authDef);

  public void visit( HttpBasicDef authDef);

  public void visit( HttpBearerDef authDef);
  }
