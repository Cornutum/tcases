//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

/**
 * Implements the Visitor pattern for {@link MocoServerConfig} objects.
 */
public interface ConfigVisitor
  {
  public void visit( MocoServerConfigFile config);

  public void visit( MocoServerConfigResource config);
  
  public void visit( MocoServerConfigPojo config);
  
  }
