//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Iterates over all {@link VarDef individual variable definitions}, using a depth-first traversal of any
 * {@link VarSet variable sets}.
 *
 */
public class VarDefIterator implements Iterator<VarDef>
  {
  /**
   * Creates a new VarDefIterator object.
   */
  public VarDefIterator( FunctionInputDef inputDef)
    {
    this( inputDef.getVarDefs());
    }

  /**
   * Creates a new VarDefIterator object.
   */
  public VarDefIterator( Iterator<IVarDef> varDefs)
    {
    varDefs_ = varDefs;
    }

  public boolean hasNext()
    {
    return getNextVarDef() != null;
    }

  public VarDef next()
    {
    if( !hasNext())
      {
      throw new NoSuchElementException();
      }

    VarDef nextVarDef = getNextVarDef();
    nextVarDef_ = null;

    return nextVarDef;
    }

  public void remove()
    {
    throw new UnsupportedOperationException();
    }

  /**
   * Returns the next
   */
  private VarDef getNextVarDef()
    {
    if( nextVarDef_ == null)
      {
      // Still traversing current VarSet?
      if( nextVarSet_ != null && nextVarSet_.hasNext())
        {
        // Yes, return next VarSet member.
        nextVarDef_ = nextVarSet_.next();
        }
      else
        {
        // No, get next variable, skipping any empty VarSet.
        nextVarSet_ = null;
        Iterator<IVarDef> members = null;
        IVarDef varDef;
        for( varDef = null;

             varDefs_.hasNext()
               && (members = (varDef = varDefs_.next()).getMembers()) != null
               && !members.hasNext();

             varDef = null);

        nextVarDef_ =
          // No VarDef left to visit?
          varDef == null? null :

          // Next variable is a VarDef?
          members == null? (VarDef) varDef :

          // No, start traversing next VarSet.
          (nextVarSet_ = new VarDefIterator( members)).next();

        }
      }

    return nextVarDef_;
    }

  private Iterator<IVarDef> varDefs_;
  private VarDef nextVarDef_;
  private VarDefIterator nextVarSet_;
  }

