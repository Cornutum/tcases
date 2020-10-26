//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.anon;

import org.cornutum.tcases.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Optional;

/**
 * Converts a system input definition into an equivalent form using anonymous identifiers.
 */
public class Anonymizer
  {
  /**
   * Translates identifiers to anonymous equivalents.
   */
  private static class Dictionary
    {
    /**
     * Creates a new Dictionary instance.
     */
    public Dictionary( FunctionInputDef inputDef)
      {
      createVarNames( inputDef);
      }

    /**
     * Creates the variable name dictionary.
     */
    private void createVarNames( FunctionInputDef inputDef)
      {
      renameVars( inputDef.getVarDefs());
      }

    /**
     * Creates the variable name dictionary.
     */
    private void renameVars( Iterator<IVarDef> varDefs)
      {
      for( int i = 0; varDefs.hasNext(); i++)
        {
        IVarDef varDef = varDefs.next();
        IVarDef parent = varDef.getParent();
        
        String anonParentPath =
          Optional.ofNullable( parent)
          .map( p -> getAnonForVarPath( p.getPathName()))
          .orElse( null);

        String anonParentName =
          Optional.ofNullable( DefUtils.toPath( anonParentPath))
          .map( path -> path[ path.length - 1])
          .orElse( null);

        String anonPath =
          Optional.ofNullable( anonParentPath)
          .map( path -> String.format( "%s.", path))
          .orElse( "");
        
        String anonPrefix =
          Optional.ofNullable( anonParentName)
          .orElse( "V");
        
        String anon = String.format( "%s%s-%s", anonPath, anonPrefix, i);

        varPathToAnon_.put( varDef.getPathName(), anon);
        anonToVarPath_.put( anon, varDef.getPathName());

        Optional.ofNullable( varDef.getMembers())
          .ifPresent( members -> renameVars( members));
        }
      }

    /**
     * Returns the anonymous synonym for the given variable path.
     */
    public String getAnonForVarPath( String varPath)
      {
      return varPathToAnon_.get( varPath);
      }

    private Map<String,String> varPathToAnon_ = new HashMap<String,String>();
    private Map<String,String> anonToVarPath_ = new HashMap<String,String>();
    }
  
  /**
   * Creates a new Anonymizer instance.
   */
  public Anonymizer()
    {
    reset();
    }

  /**
   * Converts a system input definition into an equivalent form using anonymous identifiers.
   */
  public SystemInputDef anonymize( SystemInputDef inputDef)
    {
    reset();

    SystemInputDef anonDef = new SystemInputDef( "S");

    toStream( inputDef.getFunctionInputDefs())
      .map( this::anonymize)
      .forEach( f -> anonDef.addFunctionInputDef( f));
    
    return anonDef;
    }

  /**
   * Converts a function input definition into an equivalent form using anonymous identifiers.
   */
  public FunctionInputDef anonymize( FunctionInputDef inputDef)
    {
    Dictionary dictionary = new Dictionary( inputDef);

    FunctionInputDef anonDef = new FunctionInputDef( String.format( "F%s", functionNext_++));

    return anonDef;
    }

  /**
   * Resets the state of this Anonymizer.
   */
  private void reset()
    {
    functionNext_ = 0;
    }

  private int functionNext_ = -1;
  }
