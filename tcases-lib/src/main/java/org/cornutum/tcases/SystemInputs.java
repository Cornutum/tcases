//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.resolve.DataValue.Type;
import org.cornutum.tcases.resolve.SchemaBuilder;
import org.cornutum.tcases.util.ExecutionNotifier;
import org.cornutum.tcases.util.Notifier;
import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.VarValueDef.Type.*;
import static org.cornutum.tcases.conditions.Conditions.propertiesReferenced;
import static org.cornutum.tcases.util.CollectionUtils.toOrderedSet;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.collections4.SetUtils;
import java.util.Set;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.IntStream;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

/**
 * Defines methods used to manage system input definitions.
 */
public class SystemInputs
  {
  /**
   * Returns a builder for a new {@link SystemInputDef} with the given name.
   */
  public static SystemInputDefBuilder system( String name)
    {
    return SystemInputDefBuilder.with( name);
    }

  /**
   * Returns a builder for a new {@link FunctionInputDef} with the given name.
   */
  public static FunctionInputDefBuilder function( String name)
    {
    return FunctionInputDefBuilder.with( name);
    }

  /**
   * Returns a builder for a new {@link VarSet} with the given name.
   */
  public static VarSetBuilder varSet( String name)
    {
    return VarSetBuilder.with( name);
    }

  /**
   * Returns a builder for a new {@link VarDef} with the given name.
   */
  public static VarDefBuilder var( String name)
    {
    return VarDefBuilder.with( name);
    }

  /**
   * Returns a builder for a new {@link VarValueDef} with the given name.
   */
  public static VarValueDefBuilder value( Object name)
    {
    return VarValueDefBuilder.with( name);
    }

  /**
   * Returns a builder for a new failure {@link VarValueDef} with the given name.
   */
  public static VarValueDefBuilder failureValue( Object name)
    {
    return VarValueDefBuilder.with( name).type( FAILURE);
    }

  /**
   * Returns a builder for a new once {@link VarValueDef} with the given name.
   */
  public static VarValueDefBuilder onceValue( Object name)
    {
    return VarValueDefBuilder.with( name).type( ONCE);
    }

  /**
   * Returns a builder for a new {@link org.cornutum.tcases.resolve.Schema} with the given type.
   */
  public static SchemaBuilder schema( Type type)
    {
    return SchemaBuilder.type( type);
    }

  /**
   * Returns a builder for a new generic {@link org.cornutum.tcases.resolve.Schema}.
   */
  public static SchemaBuilder schema()
    {
    return SchemaBuilder.generic();
    }

  /**
   * Creates a new SystemInputs instance.
   */
  public SystemInputs( String... startLocation)
    {
    context_ = new ProcessingContext( startLocation);
    }

  /**
   * Maps every property in the given function input definition to the variable value definitions that contribute it.
   */
  public Map<String,Set<Located<VarValueDef>>> getPropertySources( FunctionInputDef function)
    {
    return getContext().resultFor( function.getName(), () -> getPropertySources( function.getVarDefs()));
    }

  /**
   * Maps every property in the given input variable definitions to the variable value definitions that contribute it.
   */
  private Map<String,Set<Located<VarValueDef>>> getPropertySources( Iterator<IVarDef> varDefs)
    {
    return
      toStream( varDefs)
      .map( varDef -> getPropertySources( varDef))
      .collect(
        LinkedHashMap::new,
        (result, varMap) -> putAll( result, varMap),
        this::putAll);
    }

  /**
   * Maps every property in the given input variable definition to the variable value definitions that contribute it.
   */
  private Map<String,Set<Located<VarValueDef>>> getPropertySources( IVarDef varDef)
    {
    return
      getContext().resultFor( varDef.getName(), () -> {
        return
          Optional.ofNullable( varDef.getMembers())
          .map( members -> getPropertySources( members))
          .orElseGet( () -> {
            return
              toStream( varDef.getValues())
              .map( valueDef -> getPropertySources( valueDef))
              .collect(
                LinkedHashMap::new,
                (result, valueMap) -> putAll( result, valueMap),
                this::putAll);
              });
        });
    }

  /**
   * Maps every property in the given value definition to its source.
   */
  private Map<String,Set<Located<VarValueDef>>> getPropertySources( VarValueDef valueDef)
    {
    return
      getContext().resultFor( String.valueOf( valueDef.getName()), () -> {
        return
          toStream( valueDef.getProperties())
          .collect(
            LinkedHashMap::new,
            (result, property) -> put( result, property, located( valueDef)),
            this::putAll);
        });
    }
  
  /**
   * Maps every property in the given FunctionInputDef to the conditional elements that reference it.
   */
  public Map<String,Set<Located<IConditional>>> getPropertyReferences( FunctionInputDef function)
    {
    return getContext().resultFor( function.getName(), () -> getPropertyReferences( function.getVarDefs()));
    }

  /**
   * Maps every property in the given input variables to the conditional elements that reference it.
   */
  private Map<String,Set<Located<IConditional>>> getPropertyReferences( Iterator<IVarDef> varDefs)
    {
    return
      toStream( varDefs)
      .map( varDef -> getPropertyReferences( varDef))
      .collect(
        LinkedHashMap::new,
        (result, varMap) -> putAll( result, varMap),
        this::putAll);
    }

  /**
   * Maps every property in the given input variable definition to the conditional elements that reference it.
   */
  private Map<String,Set<Located<IConditional>>> getPropertyReferences( IVarDef varDef)
    {
    return
      getContext().resultFor( varDef.getName(), () -> {

        Map<String,Set<Located<IConditional>>> varRefs =
          propertiesReferenced( varDef.getCondition())
          .collect(
            LinkedHashMap::new,
            (result, property) -> put( result, property, located( varDef)),
            this::putAll);

        Map<String,Set<Located<IConditional>>> memberRefs =
          Optional.ofNullable( varDef.getMembers())
          .map( members -> getPropertyReferences( members))
          .orElseGet( () -> {
              return
              toStream( varDef.getValues())
              .map( valueDef -> getPropertyReferences( valueDef))
              .collect(
                LinkedHashMap::new,
                (result, valueMap) -> putAll( result, valueMap),
                this::putAll);
            });

        putAll( varRefs, memberRefs);
        return varRefs;
        });
    }

  /**
   * Maps every property in the given value definition to the conditional element that references it.
   */
  private Map<String,Set<Located<IConditional>>> getPropertyReferences( VarValueDef valueDef)
    {
    return
      getContext().resultFor( String.valueOf( valueDef.getName()), () -> {
        return
          propertiesReferenced( valueDef.getCondition())
          .collect(
            LinkedHashMap::new,
            (result, property) -> put( result, property, located( valueDef)),
            this::putAll);
        });
    }

  /**
   * For every variable value in the given function input definition that defines an unused
   * property, maps the value to the unused properties it defines.
   */
  public Map<Located<VarValueDef>,Set<String>> getPropertiesUnused( FunctionInputDef function)
    {
    Map<String,Set<Located<VarValueDef>>> sources = getPropertySources( function);
    Set<String> unused = SetUtils.difference( sources.keySet(), getPropertyReferences( function).keySet());

    return
      invert(
        unused.stream()
        .collect(
          toMap(
            property -> property,
            property -> sources.get( property),
            (s1, s2) -> s1,
            LinkedHashMap::new)));
    }

  /**
   * For every conditional element in the given function input definition that references an undefined property,
   * maps the element to the undefined properties that it references.
   */
  public Map<Located<IConditional>,Set<String>> getPropertiesUndefined( FunctionInputDef function)
    {
    Map<String,Set<Located<IConditional>>> refs = getPropertyReferences( function);
    Set<String> undefined = SetUtils.difference( refs.keySet(), getPropertySources( function).keySet());

    return
      invert(
        undefined.stream()
        .collect(
          toMap(
            property -> property,
            property -> refs.get( property),
            (s1, s2) -> s1,
            LinkedHashMap::new)));
    }

  /**
   * Adds a value to the set associated with the given key.
   */
  private <K,V> void put( Map<K,Set<V>> map, K key, V value)
    {
    Set<V> values = Optional.ofNullable( map.get( key)).orElseGet( LinkedHashSet::new);
    values.add( value);
    map.put( key, values);
    }

  /**
   * Merges the entries of that other map into the given map.
   */
  private <K,V> void putAll( Map<K,Set<V>> map, Map<K,Set<V>> otherMap)
    {
    otherMap.forEach( (k,v) -> v.stream().forEach( member -> put( map, k, member)));
    }

  /**
   * Returns a located value.
   */
  private <T> Located<T> located( T value)
    {
    return new Located<T>( getLocation(), value);
    }

  /**
   * Returns the inverse of the relationship represented by the given map.
   */
  private <K,V> Map<V,Set<K>> invert( Map<K,Set<V>> mapping)
    {
    List<List<V>> valueSets =
      mapping.values().stream()
      .map( values -> values.stream().collect( toList()))
      .collect( toList());

    int maxValues =
      valueSets.stream()
      .mapToInt( List::size)
      .max()
      .orElse( 0);

    Set<V> values = 
      IntStream.range( 0, maxValues)
      .mapToObj( i -> valueSets.stream().map( valueSet -> i < valueSet.size()? valueSet.get(i) : null).filter( Objects::nonNull))
      .flatMap( iths -> iths)
      .collect( toOrderedSet());
    
    return
      values.stream()
      .collect(
        LinkedHashMap::new,
        (result, value) -> mapping.keySet().stream().filter( k -> mapping.get(k).contains( value)).forEach( k -> put( result, value, k)),
        this::putAll);
    }

  /**
   * Returns the current location of system input definition processing.
   */
  private String[] getLocation()
    {
    return getContext().getLocation();
    }

  /**
   * Returns the context for system input definition processing.
   */
  private ProcessingContext getContext()
    {
    return context_;
    }

  private final ProcessingContext context_;

  /**
   * Associates a system input element with its location.
   */
  public static class Located<T>
    {
    /**
     * Creates a new Located instance.
     */
    public Located( String[] location, T element)
      {
      location_ = location;
      element_ = element;
      }

    /**
     * Returns the location of this element.
     */
    public String[] getLocation()
      {
      return location_;
      } 

    /**
     * Returns the element at this location.
     */
    public T getElement()
      {
      return element_;
      }

    @SuppressWarnings("unchecked")
	@Override
	public boolean equals( Object object)
      {
      Located<T> other =
        object != null && object.getClass().equals( getClass())
        ? (Located<T>) object
        : null;

      return
        other != null
        && Arrays.equals( other.getLocation(), getLocation())
        && Objects.equals( other.getElement(), getElement());
      }

    @Override
	public int hashCode()
      {
      return
        getClass().hashCode()
        ^ Arrays.hashCode( getLocation())
        ^ Objects.hashCode( getElement());
      }

    @Override
    public String toString()
      {
      return
        ToString.getBuilder( this)
        .append( getLocation())
        .append( getElement())
        .toString();
      }
    
    private final String[] location_;
    private final T element_;
    }

  /**
   * Defines the context for processing a {@link SystemInputDef}.
   */
  private static class ProcessingContext extends ExecutionNotifier<ProcessingException>
    {
    /**
     * Creates a new ProcessingContext instance, starting at the given location.
     */
    public ProcessingContext( String... startLocation)
      {
      super( startLocation);
      setNotifier( Notifier.ignore());
      }
    
    /**
     * Returns an exception to throw for the given failure.
     */
    @Override
    protected ProcessingException whenFailure( Throwable e)
      {
      return
        ProcessingException.class.isAssignableFrom( e.getClass())
        ? (ProcessingException) e
        : new ProcessingException( getLocation(), e);
      }

    @Override
    public String toString()
      {
      return
        ToString.getBuilder( this)
        .append( getLocation())
        .toString();
      }
    }
  
  /**
   * Reports an error processing a system input definition.
   */
  public static class ProcessingException extends RuntimeException
    {
    /**
	 * 
	 */
	private static final long serialVersionUID = -5553229742918984398L;

	/**
     * Creates a new ProcessingException instance.
     */
    public ProcessingException( String reason)
      {
      super( reason);
      }
  
    /**
     * Creates a new ProcessingException instance.
     */
    public ProcessingException( String reason, Throwable cause)
      {
      super( reason, cause);
      }
  
    /**
     * Creates a new ProcessingException instance.
     */
    public ProcessingException( String[] location, Throwable cause)
      {
      super( String.format( "Error processing %s", StringUtils.join( location, ", ")), cause);
      }
    }
  }
