//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.util.ToString;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Random;
import java.util.Set;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toMap;
import static java.util.stream.Collectors.toSet;

/**
 * Defines the properties of a set of object values that can be used by a request.
 */
public class ObjectDomain implements ValueDomain<Map<String,Object>>
  {
  /**
   * Creates a new ObjectDomain instance.
   */
  public ObjectDomain()
    {
    setPropertyDomains( null);
    setAdditionalPropertyCount( null);
    setAdditionalPropertyNames( null);
    setAdditionalPropertyValues( null);
    }

  /**
   * Changes the property value domains for objects in this domain.
   */
  public void setPropertyDomains( Map<String,ValueDomain<?>> propertyDomains)
    {
    propertyDomains_ =
      Optional.ofNullable( propertyDomains)
      .orElse( new LinkedHashMap<String,ValueDomain<?>>());
    }

  /**
   * Returns the property value domains for objects in this domain.
   */
  public Map<String,ValueDomain<?>> getPropertyDomains()
    {
    return propertyDomains_;
    }

  /**
   * Changes the value domain for the number of additional properties for this object.
   */
  public void setAdditionalPropertyCount( ValueDomain<Integer> count)
    {
    additionalPropertyCount_ =
      count == null
      ? new IntegerConstant(0)
      : count;
    }

  /**
   * Changes the value domain for any additional properties for this object.
   */
  public void setAdditionalPropertyValues( ValueDomain<?> additionalPropertyValues)
    {
    additionalPropertyValues_ =
      additionalPropertyValues == null
      ? new MultiTypeDomain( Type.any())
      : additionalPropertyValues;
    }

  /**
   * Returns the value domain for any additional properties for this object.
   */
  public ValueDomain<?> getAdditionalPropertyValues()
    {
    return additionalPropertyValues_;
    }

  /**
   * Returns the value domain for the number of additional properties for this object.
   */
  public ValueDomain<Integer> getAdditionalPropertyCount()
    {
    return additionalPropertyCount_;
    }

  /**
   * Changes the value domain for the names of any additional properties.
   */
  public void setAdditionalPropertyNames( AbstractStringDomain domain)
    {
    additionalPropertyNames_ =
      domain == null
      ? new PropertyNameDomain()
      : domain;
    }

  /**
   * Returns the value domain for the names of any additional properties.
   */
  public AbstractStringDomain getAdditionalPropertyNames()
    {
    return additionalPropertyNames_;
    }

  /**
   * Returns a random sequence of values from this domain.
   */
  public Stream<Map<String,Object>> values( Random random)
    {
    return Stream.generate( () -> newObject( random));
    }

  /**
   * Returns a new random object from this domain.
   */
  private Map<String,Object> newObject( Random random)
    {
    // Generate values for all specified properties
    Map<String,Object> object =
      getPropertyDomains().entrySet().stream()
      .collect( toMap( e -> e.getKey(), e -> e.getValue().select( random)));

    // Additional properties needed?
    int totalPropertyCount = object.size() + getAdditionalPropertyCount().select( random);
    while( object.size() < totalPropertyCount)
      {
      // Yes, add unique additional property.
      String additional;
      while( object.containsKey( (additional = getAdditionalPropertyNames().select( random))));
      object.put( additional, getAdditionalPropertyValues().select( random));
      }

    return object;
    }

  /**
   * Returns true if the given value belongs to this domain.
   */
  public boolean contains( Map<String,Object> value)
    {
    boolean containsProperties =
      getPropertyDomains().entrySet().stream()
      .allMatch( e -> value.containsKey( e.getKey()) && e.getValue().containsObject( value.get( e.getKey())));

    if( containsProperties)
      {
      Set<String> additionalProperties =
        value.keySet().stream()
        .filter( p -> !getPropertyDomains().containsKey( p))
        .collect( toSet());

      containsProperties =
        getAdditionalPropertyCount().contains( additionalProperties.size())
        &&
        additionalProperties.stream()
        .allMatch(
          p ->
          getAdditionalPropertyNames().contains( p)
          && getAdditionalPropertyValues().containsObject( value.get(p)));
      }
      
    return containsProperties;
    }

  /**
   * Return the type(s) of values that belong to this domain.
   */
  public Type[] getTypes()
    {
    return Type.only( Type.OBJECT);
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "properties", getPropertyDomains().keySet())
      .append( "additional", getAdditionalPropertyCount())
      .toString();
    }

  private Map<String,ValueDomain<?>> propertyDomains_;
  private ValueDomain<Integer> additionalPropertyCount_;
  private AbstractStringDomain additionalPropertyNames_;
  private ValueDomain<?> additionalPropertyValues_;
  }
