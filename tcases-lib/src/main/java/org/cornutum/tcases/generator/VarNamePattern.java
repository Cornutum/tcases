//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.*;

import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.Iterator;

/**
 * Represents a pattern matching one or more input variable names.
 *
 */
public class VarNamePattern
  {
  /**
   * Creates a new VarNamePattern object.
   */
  public VarNamePattern( String pattern)
    {
    varNamePath_ =
      StringUtils.isBlank( pattern)
      ? null
      : DefUtils.toPath( pattern);

    int patternLength = varNamePath_==null? 0 : varNamePath_.length;
    for( minDepth_ = 0,
           wildcard_ = false;

         minDepth_ < patternLength
           && !(wildcard_ = ALL_DESCENDANTS.equals( varNamePath_[ minDepth_]) || ALL_CHILDREN.equals( varNamePath_[ minDepth_]));
           
         minDepth_++);
    }


  /**
   * Returns true if the given variable name matches this pattern.
   */
  public boolean matches( String varName)
    {
    return matches( new VarNamePattern( varName));
    }
  
  /**
   * Returns true if the given variable name matches this pattern.
   */
  public boolean matches( VarNamePattern varName)
    {
    boolean matched =
      // Variable name defined?
      varName != null

      // Variable name and this pattern both (non-)empty?
      && (varName.varNamePath_ == null) == (varNamePath_ == null)

      // Variable name identifies single variable of depth no less than this pattern?
      && !varName.wildcard_
      && varName.minDepth_ >= minDepth_;

    if( matched && varNamePath_ != null)
      {
      // Variable name matches this pattern up to wildcard?
      int i;
      for( i = 0;

           i < minDepth_
             && (matched = varName.varNamePath_[i].equals( varNamePath_[i]));

           i++);

      if( matched)
        {
        int membersLeft = varName.minDepth_ - minDepth_;

        String wildcardType =
          !wildcard_
          ? null
          : varNamePath_[i];
          
        matched =
          // No wildcard to match?
          wildcardType == null
          ? (membersLeft == 0)

          // This variable is a matching child?
          : ((membersLeft == 1 && wildcardType.equals( ALL_CHILDREN))
             ||
             // This variable is a matching descendant?
             wildcardType.equals( ALL_DESCENDANTS));
        }
      }
      
    return matched;
    }

  /**
   * Returns true if this pattern is valid.
   */
  public boolean isValid()
    {
    // Must be non-empty...
    boolean valid = varNamePath_ != null;

    if( valid)
      {
      // Containing a sequence of identifiers...
      int i;
      for( i = 0; i < varNamePath_.length && DefUtils.isIdentifier( varNamePath_[i]); i++);

      // Optionally terminated by a single wildcard.
      int membersLeft = varNamePath_.length - i;
      valid =
        membersLeft == 0
        ||
        (membersLeft == 1
         &&
         (varNamePath_[i].equals( ALL_DESCENDANTS) || varNamePath_[i].equals( ALL_CHILDREN)));
      }

    return valid;
    }

  /**
   * Returns true if this pattern is applicable to the given input definition.
   */
  public boolean isApplicable( FunctionInputDef inputDef)
    {
    return isApplicable( inputDef.getVarDefs());
    }

  /**
   * Returns true if this pattern is applicable to the given set of variables.
   */
  private boolean isApplicable( Iterator<IVarDef> varDefs)
    {
    boolean applicable = false;
    while( varDefs.hasNext() && !(applicable = isApplicable( varDefs.next())));
    return applicable;
    }

  /**
   * Returns true if this pattern is applicable to the given variable.
   */
  private boolean isApplicable( IVarDef var)
    {
    Iterator<IVarDef> members = var.getMembers();

    return
      members == null
      ? matches( var.getPathName())
      : isApplicable( members);
    }

  public boolean equals( Object object)
    {
    return
      object != null
      && object.getClass().equals( getClass())
      && Arrays.equals( varNamePath_, ((VarNamePattern) object).varNamePath_);
    }

  public int hashCode()
    {
    int code = getClass().hashCode();
    int pathSize = varNamePath_==null? 0 : varNamePath_.length;

    for( int i = 0;
         i < pathSize;
         code ^= varNamePath_[i++].hashCode());

    return code;
    }

  public String toString()
    {
    return StringUtils.join( varNamePath_, '.');
    }
    
  private String[] varNamePath_;
  private int minDepth_;
  private boolean wildcard_;
    
  private static final String ALL_DESCENDANTS = "**";
  private static final String ALL_CHILDREN    = "*";
  }
