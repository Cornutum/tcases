//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.anon;

import org.apache.commons.lang3.StringUtils;
import org.cornutum.tcases.*;
import org.cornutum.tcases.conditions.*;
import static org.cornutum.tcases.conditions.Conditions.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Optional;
import static java.util.stream.Collectors.toCollection;
import static java.util.stream.Collectors.toList;

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
      createPropertyNames( inputDef);
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

        Optional.ofNullable( varDef.getMembers())
          .ifPresent( members -> renameVars( members));
        }
      }

    /**
     * Creates the property name dictionary.
     */
    private void createPropertyNames( FunctionInputDef inputDef)
      {
      String[] properties = 
        toStream( new VarDefIterator( inputDef))
        .flatMap( varDef -> toStream( varDef.getValues()))
        .flatMap( valueDef -> toStream( valueDef.getProperties().iterator()).sorted())
        .collect( toCollection( LinkedHashSet<String>::new))
        .toArray( new String[0]);

      int maxLen = String.valueOf( properties.length).length();
      for( int i = 0; i < properties.length; i++)
        {
        propertyToAnon_.put( properties[i], String.format( "P-%s", StringUtils.leftPad( String.valueOf(i), maxLen, '0')));
        }
      }

    /**
     * Returns the anonymous synonym for the given variable path.
     */
    public String getAnonForVarPath( String varPath)
      {
      return varPathToAnon_.get( varPath);
      }

    /**
     * Returns the anonymous synonym for the given property.
     */
    public String getAnonForProperty( String property)
      {
      return propertyToAnon_.get( property);
      }

    private Map<String,String> varPathToAnon_ = new HashMap<String,String>();
    private Map<String,String> propertyToAnon_ = new HashMap<String,String>();
    }

  /**
   * An {@link IConditionVisitor} that anonymizes a condition.
   */
  private static class ConditionAnonymizer implements IConditionVisitor
    {
    /**
     * Returns an anonymous version of the given condition.
     */
    public static ICondition anonymize( Dictionary dictionary, ICondition condition)
      {
      ICondition anonCondition = null;
      if( condition != null)
        {
        ConditionAnonymizer anonymizer = new ConditionAnonymizer( dictionary);
        condition.accept( anonymizer);
        anonCondition = anonymizer.anonCondition_;
        }
      
      return anonCondition;
      }

    /**
     * Creates a new ConditionAnonymizer instance.
     */
    public ConditionAnonymizer( Dictionary dictionary)
      {
      dictionary_ = dictionary;
      }

    public void visit( AllOf condition)
      {
      anonCondition_ =
        allOf(
          toStream( condition.getConditions())
          .map( c -> anonymize( dictionary_, c))
          .toArray( ICondition[]::new));
      }
  
    public void visit( AnyOf condition)
      {
      anonCondition_ =
        anyOf(
          toStream( condition.getConditions())
          .map( c -> anonymize( dictionary_, c))
          .toArray( ICondition[]::new));
      }
  
    public void visit( ContainsAll condition)
      {
      anonCondition_ =
        has(
          toStream( condition.getProperties())
          .map( p -> dictionary_.getAnonForProperty( p))
          .toArray( String[]::new));
      }
  
    public void visit( ContainsAny condition)
      {
      anonCondition_ =
        hasAny(
          toStream( condition.getProperties())
          .map( p -> dictionary_.getAnonForProperty( p))
          .toArray( String[]::new));
      }
  
    public void visit( IConjunct condition)
      {
      }
  
    public void visit( Not condition)
      {
      anonCondition_ =
        not(
          toStream( condition.getConditions())
          .map( c -> anonymize( dictionary_, c))
          .toArray( ICondition[]::new));
      }

    public void visit( AssertLess condition)
      {
      anonCondition_ =
        lessThan(
          dictionary_.getAnonForProperty( condition.getProperty()),
          condition.getBound());
      }

    public void visit( AssertMore condition)
      {
      anonCondition_ =
        moreThan(
          dictionary_.getAnonForProperty( condition.getProperty()),
          condition.getBound());
      }

    public void visit( AssertNotLess condition)
      {
      anonCondition_ =
        notLessThan(
          dictionary_.getAnonForProperty( condition.getProperty()),
          condition.getBound());
      }

    public void visit( AssertNotMore condition)
      {
      anonCondition_ =
        notMoreThan(
          dictionary_.getAnonForProperty( condition.getProperty()),
          condition.getBound());
      }

    public void visit( Between condition)
      {
      anonCondition_ =
        new Between(
          (BoundedAssertion) anonymize( dictionary_, condition.getMin()),
          (BoundedAssertion) anonymize( dictionary_, condition.getMax()));
      }

    public void visit( Equals condition)
      {
      anonCondition_ =
        equalTo(
          dictionary_.getAnonForProperty( condition.getMin().getProperty()),
          condition.getMin().getBound());
      }

    private Dictionary dictionary_;
    private ICondition anonCondition_;
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
    anonymizeVars( dictionary, inputDef, anonDef);
    anonymizeValues( dictionary, inputDef, anonDef);

    return anonDef;
    }

  /**
   * Adds an anonymized version of each variable in <CODE>inputDef</CODE> to the given <CODE>anonDef</CODE>.
   */
  private void anonymizeVars( Dictionary dictionary, FunctionInputDef inputDef, FunctionInputDef anonDef)
    {
    toStream( new VarDefIterator( inputDef))
      .forEach( varDef -> anonymizeVar( dictionary, anonDef, varDef));
    }

  /**
   * Adds an anonymized version of the given variable to the given <CODE>anonDef</CODE>.
   */
  private IVarDef anonymizeVar( Dictionary dictionary, FunctionInputDef anonDef, IVarDef var)
    {
    String anonVarPath = dictionary.getAnonForVarPath( var.getPathName());
    IVarDef anonVar = anonDef.findVarPath( anonVarPath);
    if( anonVar == null)
      {
      String anonName =
        Optional.of( DefUtils.toPath( anonVarPath))
        .map( path -> path[ path.length - 1])
        .get();

      if( var.getMembers() == null)
        {
        anonVar =
          VarDefBuilder.with( anonName)
          .type( var.getType())
          .when( ConditionAnonymizer.anonymize( dictionary, var.getCondition()))
          .build();
        }
      else
        {
        anonVar =
          VarSetBuilder.with( anonName)
          .type( var.getType())
          .when( ConditionAnonymizer.anonymize( dictionary, var.getCondition()))
          .build();
        }
    
      IVarDef parent = var.getParent();
      if( parent == null)
        {
        anonDef.addVarDef( anonVar);
        }
      else
        {
        VarSet anonParent = (VarSet) anonymizeVar( dictionary, anonDef, parent);
        anonParent.addMember( anonVar);
        }
      }

    return anonVar;
    }

  /**
   * Adds an anonymized version of each variable value in <CODE>inputDef</CODE> to the given <CODE>anonDef</CODE>.
   */
  private void anonymizeValues( Dictionary dictionary, FunctionInputDef inputDef, FunctionInputDef anonDef)
    {
    toStream( new VarDefIterator( inputDef))
      .forEach( varDef -> anonymizeValues( dictionary, inputDef, anonDef, varDef));
    }

  /**
   * Adds an anonymized version of each value of the given variable in <CODE>inputDef</CODE> to the given <CODE>anonDef</CODE>.
   */
  private void anonymizeValues( Dictionary dictionary, FunctionInputDef inputDef, FunctionInputDef anonDef, VarDef varDef)
    {
    String anonVarPath = dictionary.getAnonForVarPath( varDef.getPathName());

    String anonValueTag =
      Optional.of( DefUtils.toPath( anonVarPath))
      .map( path -> path[ path.length - 1])
      .map( name -> name.replaceAll( "V", "L"))
      .get();

    VarDef anonVar = anonDef.findVarDefPath( anonVarPath);
    Iterator<VarValueDef> values = varDef.getValues();
    for( int i = 0; values.hasNext(); i++)
      {
      VarValueDef value = values.next();
      Object valueName = value.getName();
      Object anonValueName = valueName instanceof String? String.format( "%s_%s", anonValueTag, i) : valueName;
      anonVar.addValue(
        VarValueDefBuilder.with( anonValueName)
        .type( value.getType())
        .when( ConditionAnonymizer.anonymize( dictionary, value.getCondition()))
        .properties( toStream( value.getProperties()).map( p -> dictionary.getAnonForProperty( p)).collect( toList()))
        .build());
      }
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
