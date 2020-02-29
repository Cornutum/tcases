//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.util.Set;
import java.util.stream.Stream;

/**
 * Defines a set of string values that can be used by a request.
 */
public abstract class AbstractStringDomain extends SequenceDomain<String>
  {
  /**
   * Creates a new AbstractStringDomain instance.
   */
  protected AbstractStringDomain( int maxLength)
    {
    super( maxLength);
    }

  /**
   * Changes the values excluded from this domain.
   */
  public void setExcludedStrings( Set<String> excluded)
    {
    setExcluded( excluded);
    }
  
  /**
   * Returns the length of the given value.
   */
  protected int getLength( String value)
    {
    return value.length();
    }

  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected DataValue<String> dataValueOf( String value)
    {
    return new StringValue( value);
    }

  /**
   * Returns a new random string.
   */
  protected String newValue( ResolverContext context)
    {
    return newValue( context, getLengthRange().selectValue( context));
    }

  /**
   * Returns a new random string of the given length for this domain.
   */
  protected abstract String newValue( ResolverContext context, int length);
  
  /**
   * Returns a random sequence of possible members of this domain.
   */
  protected Stream<String> candidates( ResolverContext context)
    {
    return Stream.generate( () -> newValue( context));
    }
}
