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
import org.cornutum.tcases.generator.*;
import static org.cornutum.tcases.conditions.Conditions.*;
import static org.cornutum.tcases.util.CollectionUtils.toOrderedSet;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Optional;
import static java.util.stream.Collectors.toList;

/**
 * Converts a system input definition into an equivalent form using anonymous identifiers.
 */
public class Anonymizer
  {
  /**
   * Translates identifiers to anonymous equivalents.
   */
  private static class SystemDictionary
    {
    /**
     * Creates a new SystemDictionary instance.
     */
    public SystemDictionary( SystemInputDef inputDef)
      {
      anonSystem_ = "S";
      Iterator<FunctionInputDef> functionDefs = inputDef.getFunctionInputDefs();
      for( int i = 0; functionDefs.hasNext(); i++)
        {
        FunctionInputDef functionDef = functionDefs.next();
        String anonFunction = String.format( "F%s", i);
        functionToDict_.put( functionDef.getName(), new FunctionDictionary( anonFunction, functionDef));
        }
      }

    /**
     * Returns the anonymous name for this system.
     */
    public String getAnonSystem()
      {
      return anonSystem_;
      }

    /**
     * Returns the dictionary for the given function.
     */
    public FunctionDictionary getDictForFunction( String function)
      {
      FunctionDictionary dictionary;
      if( !GeneratorSet.ALL.equals( function))
        {
        dictionary = functionToDict_.get( function);
        }
      else if( functionToDict_.size() != 1)
        {
        throw new IllegalStateException( String.format( "%s functions associated with generator=%s", functionToDict_.size(), function));
        }
      else
        {
        // Can anonymize an "all functions" generator only if exactly one function defined
        dictionary = new FunctionDictionary( GeneratorSet.ALL, functionToDict_.values().iterator().next());
        }
      
      return dictionary;
      }

    private final String anonSystem_;
    private final Map<String,FunctionDictionary> functionToDict_ = new HashMap<String,FunctionDictionary>();
    }
  
  /**
   * Translates identifiers to anonymous equivalents.
   */
  private static class FunctionDictionary
    {
    /**
     * Creates a new FunctionDictionary instance.
     */
    public FunctionDictionary( String anonFunction, FunctionInputDef inputDef)
      {
      anonFunction_ = anonFunction;
      createVarNames( inputDef);
      createValueNames( inputDef);
      createPropertyNames( inputDef);
      }
    
    /**
     * Creates a new FunctionDictionary instance.
     */
    public FunctionDictionary( String anonFunction, FunctionDictionary other)
      {
      anonFunction_ = anonFunction;
      varPathToAnon_ = other.varPathToAnon_;
      varPathValuesToAnon_ = other.varPathValuesToAnon_;
      propertyToAnon_ = other.propertyToAnon_;
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
     * Creates the value name dictionary.
     */
    private void createValueNames( FunctionInputDef inputDef)
      {
      toStream( new VarDefIterator( inputDef))
        .forEach( varDef -> {
          String anonVarPath = getAnonForVarPath( varDef.getPathName());

          String anonValueTag =
            Optional.of( DefUtils.toPath( anonVarPath))
            .map( path -> path[ path.length - 1])
            .map( name -> name.replaceAll( "V", "L"))
            .get();

          Iterator<VarValueDef> values = varDef.getValues();
          Map<Object,Object> anonValues = new HashMap<Object,Object>();
          for( int i = 0; values.hasNext(); i++)
            {
            VarValueDef value = values.next();

            Object valueName = value.getName();
            Object anonValueName =
              valueName == null || valueName instanceof Number || valueName instanceof Boolean
              ? valueName
              : String.format( "%s_%s", anonValueTag, i);

            anonValues.put( valueName, anonValueName);
            }

          varPathValuesToAnon_.put( varDef.getPathName(), anonValues);
          });
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
        .collect( toOrderedSet())
        .toArray( new String[0]);

      int maxLen = String.valueOf( properties.length).length();
      for( int i = 0; i < properties.length; i++)
        {
        propertyToAnon_.put( properties[i], String.format( "P-%s", StringUtils.leftPad( String.valueOf(i), maxLen, '0')));
        }
      }

    /**
     * Returns the anonymous name for this function.
     */
    public String getAnonFunction()
      {
      return anonFunction_;
      }

    /**
     * Returns the anonymous synonym for the given variable path.
     */
    public String getAnonForVarPath( String varPath)
      {
      return varPathToAnon_.get( varPath);
      }

    /**
     * Returns the anonymous synonym for the given variable value.
     */
    public Object getAnonForValue( String varPath, Object value)
      {
      return varPathValuesToAnon_.get( varPath).get( value);
      }

    /**
     * Returns the anonymous synonym for the given property.
     */
    public String getAnonForProperty( String property)
      {
      return propertyToAnon_.get( property);
      }

    private final String anonFunction_;
    private Map<String,String> varPathToAnon_ = new HashMap<String,String>();
    private Map<String,Map<Object,Object>> varPathValuesToAnon_ = new HashMap<String,Map<Object,Object>>();
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
    public static ICondition anonymize( FunctionDictionary dictionary, ICondition condition)
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
    public ConditionAnonymizer( FunctionDictionary dictionary)
      {
      dictionary_ = dictionary;
      }

    @Override
    public void visit( AllOf condition)
      {
      anonCondition_ =
        allOf(
          toStream( condition.getConditions())
          .map( c -> anonymize( dictionary_, c))
          .toArray( ICondition[]::new));
      }
  
    @Override
    public void visit( AnyOf condition)
      {
      anonCondition_ =
        anyOf(
          toStream( condition.getConditions())
          .map( c -> anonymize( dictionary_, c))
          .toArray( ICondition[]::new));
      }
  
    @Override
    public void visit( ContainsAll condition)
      {
      anonCondition_ =
        has(
          toStream( condition.getProperties())
          .map( p -> dictionary_.getAnonForProperty( p))
          .toArray( String[]::new));
      }
  
    @Override
    public void visit( ContainsAny condition)
      {
      anonCondition_ =
        hasAny(
          toStream( condition.getProperties())
          .map( p -> dictionary_.getAnonForProperty( p))
          .toArray( String[]::new));
      }
  
    @Override
    public void visit( IConjunct condition)
      {
      }
  
    @Override
    public void visit( Not condition)
      {
      anonCondition_ =
        not(
          toStream( condition.getConditions())
          .map( c -> anonymize( dictionary_, c))
          .toArray( ICondition[]::new));
      }

    @Override
    public void visit( AssertLess condition)
      {
      anonCondition_ =
        lessThan(
          dictionary_.getAnonForProperty( condition.getProperty()),
          condition.getBound());
      }

    @Override
    public void visit( AssertMore condition)
      {
      anonCondition_ =
        moreThan(
          dictionary_.getAnonForProperty( condition.getProperty()),
          condition.getBound());
      }

    @Override
    public void visit( AssertNotLess condition)
      {
      anonCondition_ =
        notLessThan(
          dictionary_.getAnonForProperty( condition.getProperty()),
          condition.getBound());
      }

    @Override
    public void visit( AssertNotMore condition)
      {
      anonCondition_ =
        notMoreThan(
          dictionary_.getAnonForProperty( condition.getProperty()),
          condition.getBound());
      }

    @Override
    public void visit( Between condition)
      {
      anonCondition_ =
        new Between(
          (BoundedAssertion) anonymize( dictionary_, condition.getMin()),
          (BoundedAssertion) anonymize( dictionary_, condition.getMax()));
      }

    @Override
    public void visit( Equals condition)
      {
      anonCondition_ =
        equalTo(
          dictionary_.getAnonForProperty( condition.getMin().getProperty()),
          condition.getMin().getBound());
      }

    private FunctionDictionary dictionary_;
    private ICondition anonCondition_;
    }

  /**
   * Creates a new Anonymizer instance.
   */
  public Anonymizer( SystemInputDef inputDef)
    {
    dictionary_ = new SystemDictionary( inputDef);
    anonDef_ = anonymize( inputDef);
    }

  /**
   * Returns an anonymized system input definition.
   */
  public SystemInputDef getInputDef()
    {
    return anonDef_;
    }

  /**
   * Converts a system input definition into an equivalent form using anonymous identifiers.
   */
  private SystemInputDef anonymize( SystemInputDef inputDef)
    {
    SystemInputDef anonDef = new SystemInputDef( dictionary_.getAnonSystem());

    toStream( inputDef.getFunctionInputDefs())
      .map( f -> anonymize( dictionary_.getDictForFunction( f.getName()), f))
      .forEach( f -> anonDef.addFunctionInputDef( f));
    
    return anonDef;
    }

  /**
   * Converts a function input definition into an equivalent form using anonymous identifiers.
   */
  private FunctionInputDef anonymize( FunctionDictionary dictionary, FunctionInputDef inputDef)
    {
    FunctionInputDef anonDef = new FunctionInputDef( dictionary.getAnonFunction());
    anonymizeVars( dictionary, inputDef, anonDef);
    anonymizeValues( dictionary, inputDef, anonDef);

    return anonDef;
    }

  /**
   * Adds an anonymized version of each variable in <CODE>inputDef</CODE> to the given <CODE>anonDef</CODE>.
   */
  private void anonymizeVars( FunctionDictionary dictionary, FunctionInputDef inputDef, FunctionInputDef anonDef)
    {
    toStream( new VarDefIterator( inputDef))
      .forEach( varDef -> anonymizeVar( dictionary, anonDef, varDef));
    }

  /**
   * Adds an anonymized version of the given variable to the given <CODE>anonDef</CODE>.
   */
  private IVarDef anonymizeVar( FunctionDictionary dictionary, FunctionInputDef anonDef, IVarDef var)
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
  private void anonymizeValues( FunctionDictionary dictionary, FunctionInputDef inputDef, FunctionInputDef anonDef)
    {
    toStream( new VarDefIterator( inputDef))
      .forEach( varDef -> anonymizeValues( dictionary, inputDef, anonDef, varDef));
    }

  /**
   * Adds an anonymized version of each value of the given variable in <CODE>inputDef</CODE> to the given <CODE>anonDef</CODE>.
   */
  private void anonymizeValues( FunctionDictionary dictionary, FunctionInputDef inputDef, FunctionInputDef anonDef, VarDef varDef)
    {
    String varPath = varDef.getPathName();
    String anonVarPath = dictionary.getAnonForVarPath( varPath);
    VarDef anonVar = anonDef.findVarDefPath( anonVarPath);

    toStream( varDef.getValues())
      .forEach( value -> {
        anonVar.addValue(
          VarValueDefBuilder.with( dictionary.getAnonForValue( varPath, value.getName()))
          .type( value.getType())
          .when( ConditionAnonymizer.anonymize( dictionary, value.getCondition()))
          .properties( toStream( value.getProperties()).map( p -> dictionary.getAnonForProperty( p)).collect( toList()))
          .build());
        });
    }

  /**
   * Converts a generator set into an equivalent form using anonymous identifiers.
   */
  public IGeneratorSet anonymize( IGeneratorSet genDef)
    {
    GeneratorSet anonDef = new GeneratorSet();

    Arrays.stream( genDef.getGeneratorFunctions())
      .forEach( f -> {
        FunctionDictionary functionDict = dictionary_.getDictForFunction( f);
        anonDef.addGenerator( functionDict.getAnonFunction(), anonymize( functionDict, (TupleGenerator) genDef.getGenerator( f)));
        });
      
    return anonDef;
    }

  /**
   * Converts a {@link TupleGenerator} into an equivalent form using anonymous identifiers.
   */
  private TupleGenerator anonymize( FunctionDictionary dictionary, TupleGenerator tupleGen)
    {
    TupleGenerator anonGen = new TupleGenerator();

    anonGen.setDefaultTupleSize( tupleGen.getDefaultTupleSize());
    anonGen.setRandomSeed( tupleGen.getRandomSeed());
    anonGen.setCombiners( tupleGen.getCombiners().stream().map( c -> anonymize( dictionary, c)).collect( toList()));

    return anonGen;
    }

  /**
   * Converts a {@link TupleCombiner} into an equivalent form using anonymous identifiers.
   */
  private TupleCombiner anonymize( FunctionDictionary dictionary, TupleCombiner tupleCombiner)
    {
    TupleCombiner anonCombiner = new TupleCombiner();

    anonCombiner.setTupleSize( tupleCombiner.getTupleSize());

    Arrays.stream( tupleCombiner.getIncluded())
      .forEach( varRef -> anonCombiner.addIncludedVar( anonymizeVarRef( dictionary, varRef)));
    Arrays.stream( tupleCombiner.getExcluded())
      .forEach( varRef -> anonCombiner.addExcludedVar( anonymizeVarRef( dictionary, varRef)));
    toStream( tupleCombiner.getOnceTuples())
      .forEach( tupleRef -> anonCombiner.addOnceTuple( anonymizeTupleRef( dictionary, tupleRef)));

    return anonCombiner;
    }

  /**
   * Converts a variable reference into an equivalent form using anonymous identifiers.
   */
  private String anonymizeVarRef( FunctionDictionary dictionary, String varRef)
    {
    int wildcardStart = varRef.lastIndexOf( ".*");

    String varPath =
      wildcardStart >= 0
      ? varRef.substring( 0, wildcardStart)
      : varRef;

    String wildcard =
      wildcardStart >= 0
      ? varRef.substring( wildcardStart)
      : "";
    
    return String.format( "%s%s", dictionary.getAnonForVarPath( varPath), wildcard);
    }

  /**
   * Converts a tuple reference into an equivalent form using anonymous identifiers.
   */
  private TupleRef anonymizeTupleRef( FunctionDictionary dictionary, TupleRef tupleRef)
    {
    TupleRef anonRef = new TupleRef();

    toStream( tupleRef.getVarBindings())
      .map( binding -> {
        String anonVar = dictionary.getAnonForVarPath( binding.getVar());
        return new VarBinding( anonVar, dictionary.getAnonForValue( binding.getVar(), binding.getValue()));
        })
      .forEach( binding -> anonRef.addVarBinding( binding));
    
    return anonRef;
    }

  private final SystemDictionary dictionary_;
  private final SystemInputDef anonDef_;
  }
