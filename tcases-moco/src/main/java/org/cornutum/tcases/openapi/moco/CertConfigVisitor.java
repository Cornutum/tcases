//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

/**
 * Implements the Visitor pattern for {@link CertConfig} objects.
 */
public interface CertConfigVisitor
  {
  public void visit( CertConfigFile config);

  public void visit( CertConfigResource config);
  }
