//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;


/**
 * Defines constants for the identifiers in a {@link org.cornutum.tcases.generator.TupleGenerator}
 * definition document.
 *
 * @version $Revision$, $Date$
 */
public final class TupleGeneratorDoc
  {
  private TupleGeneratorDoc()
    {
    }

  public static final String COMBINE_TAG         = "Combine";
  public static final String EXCLUDE_TAG         = "Exclude";
  public static final String INCLUDE_TAG         = "Include";
  public static final String TUPLEGENERATOR_TAG  = "TupleGenerator";

  public static final String FUNCTION_ATR        = "function";
  public static final String SEED_ATR            = "seed";
  public static final String TUPLES_ATR          = "tuples";
  public static final String VAR_ATR             = "var";
  }
