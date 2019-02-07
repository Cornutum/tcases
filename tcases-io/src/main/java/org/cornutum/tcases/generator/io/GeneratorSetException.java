//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

/**
 * Reports an error reading a generator set definition.
 */
public class GeneratorSetException extends RuntimeException
  {
  private static final long serialVersionUID = -292476661983857314L;

  /**
   * Creates a new GeneratorSetException instance.
   */
  public GeneratorSetException( String reason)
    {
    super( reason);
    }
  
  /**
   * Creates a new GeneratorSetException instance.
   */
  public GeneratorSetException( String reason, Throwable cause)
    {
    super( reason, cause);
    }
  }
