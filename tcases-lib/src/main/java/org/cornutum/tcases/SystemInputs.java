//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import static org.cornutum.tcases.conditions.Conditions.propertiesReferenced;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toMap;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MultiMapUtils;
import org.apache.commons.collections4.SetValuedMap;

/**
 * Defines methods used to manage system input definitions.
 */
public final class SystemInputs
  {
  /**
   * Creates a new SystemInputs instance.
   */
  private SystemInputs()
    {
    // Static methods only.
    }

  /**
   * Maps every property in the given function input definition to the variable value definitions that contribute it.
   */
  public static Map<String,Collection<VarBindingDef>> getPropertySources( FunctionInputDef function)
    {
    return getPropertySources( function.getVarDefs());
    }

  /**
   * Maps every property in the given input variable definitions to the variable value definitions that contribute it.
   */
  public static Map<String,Collection<VarBindingDef>> getPropertySources( Iterator<IVarDef> varDefs)
    {
    return getPropertySources( toStream( new VarDefIterator( varDefs)));
    }

  /**
   * Maps every property in the given input variable definitions to the variable value definitions that contribute it.
   */
  public static Map<String,Collection<VarBindingDef>> getPropertySources( Stream<VarDef> varDefs)
    {
    SetValuedMap<String,VarBindingDef> sources = MultiMapUtils.newSetValuedHashMap();

    varDefs
      .flatMap( var -> toStream( var.getValues()).map( value -> new VarBindingDef( var, value)))
      .forEach( binding -> toStream( binding.getValueDef().getProperties()).forEach( p -> sources.put( p, binding)));
    
    return sources.asMap();
    }

  /**
   * Maps every property in the given FunctionInputDef to the conditional elements that reference it.
   */
  public static Map<String,Collection<IConditional>> getPropertyReferences( FunctionInputDef function)
    {
    return getPropertyReferences( function.getVarDefs());
    }

  /**
   * Maps every property in the given input variables to the conditional elements that reference it.
   */
  public static Map<String,Collection<IConditional>> getPropertyReferences( Iterator<IVarDef> varDefs)
    {
    return getPropertyReferences( toStream( varDefs));
    }

  /**
   * Maps every property in the given input variables to the conditional elements that reference it.
   */
  public static Map<String,Collection<IConditional>> getPropertyReferences( Stream<IVarDef> varDefs)
    {
    SetValuedMap<String,IConditional> refs = MultiMapUtils.newSetValuedHashMap();

    conditionals( varDefs)
      .forEach( conditional -> propertiesReferenced( conditional.getCondition()).forEach( p -> refs.put( p, conditional)));
    
    return refs.asMap();
    }

  /**
   * For every property in the given function input definition that is defined but never referenced,
   * maps the property to the variable value definitions that contribute it.
   */
  public static Map<String,Collection<VarBindingDef>> getPropertiesUnused( FunctionInputDef function)
    {
    Map<String,Collection<VarBindingDef>> sources = getPropertySources( function);
    Collection<String> unused = CollectionUtils.subtract( sources.keySet(), getPropertyReferences( function).keySet());

    return
      sources.entrySet().stream()
      .filter( entry -> unused.contains( entry.getKey()))
      .collect( toMap( entry -> entry.getKey(), entry -> entry.getValue()));
    }

  /**
   * For every property in the given function input definition that is referenced but never defined,
   * maps the property to the conditional elements that reference it.
   */
  public static Map<String,Collection<IConditional>> getPropertiesUndefined( FunctionInputDef function)
    {
    Map<String,Collection<IConditional>> refs = getPropertyReferences( function);
    Collection<String> undefined = CollectionUtils.subtract( refs.keySet(), getPropertySources( function).keySet());

    return
      refs.entrySet().stream()
      .filter( entry -> undefined.contains( entry.getKey()))
      .collect( toMap( entry -> entry.getKey(), entry -> entry.getValue()));
    }

  /**
   * Returns the full reference name for the given IConditional.
   */
  public static String getReferenceName( IConditional conditional)
    {
    return
      conditional instanceof IVarDef?
      String.format
      ( "variable=%s",
        ((IVarDef) conditional).getPathName()) :

      conditional instanceof VarBindingDef?
      String.format
      ( "variable=%s, value=%s",
        ((VarBindingDef) conditional).getVarDef().getPathName(),
        String.valueOf( ((VarBindingDef) conditional).getValueDef().getName())) :

      null;
    }

  /**
   * Returns the IConditional instances defined by the given input variable definitions.
   */
  private static Stream<IConditional> conditionals( Stream<IVarDef> varDefs)
    {
    return varDefs.flatMap( var -> conditionals( var));
    }

  /**
   * Returns the IConditional instances defined by the given variable definition.
   */
  private static Stream<IConditional> conditionals( IVarDef var)
    {
    return
      Stream.concat
      ( Stream.of( var),
        Stream.concat
        ( Optional.ofNullable( var.getMembers()).map( members -> toStream( members).flatMap( member -> conditionals( member))).orElse( Stream.empty()),
          Optional.ofNullable( var.getValues()).map( values -> toStream( values).map( value -> new VarBindingDef( (VarDef)var, value))).orElse( Stream.empty())));
    }

  }
