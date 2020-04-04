//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import static java.util.Collections.unmodifiableList;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toCollection;

/**
 * Describes executable test cases for the API requests defined by an OpenAPI spec.
 */
public class RequestTestDef
  {
  /**
   * Creates a new RequestTestDef instance.
   */
  public RequestTestDef()
    {
    }
  
  /**
   * Creates a new RequestTestDef instance.
   */
  public RequestTestDef( Iterable<RequestCase> requestCases)
    {
    if( requestCases != null)
      {
      for( RequestCase requestCase : requestCases)
        {
        add( requestCase);
        }
      }
    }

  /**
   * Adds an API request test case.
   */
  public RequestTestDef add( RequestCase requestCase)
    {
    if( requestCase != null)
      {
      int i = requestCases_.indexOf( requestCase);
      if( i >= 0)
        {
        throw new IllegalArgumentException( String.format( "%s is already defined", requestCases_.get(i)));
        }
      requestCases_.add( requestCase);
      }

    return this;
    }

  /**
   * Removes an API request test case.
   */
  public RequestTestDef remove( RequestCase requestCase)
    {
    if( requestCase != null)
      {
      requestCases_.remove( requestCase);
      }

    return this;
    }

  /**
   * Returns all API request test cases.
   */
  public List<RequestCase> getRequestCases()
    {
    return unmodifiableList( requestCases_);
    }

  /**
   * Returns all API request test cases for the given resource path.
   */
  public List<RequestCase> getRequestCases( String path)
    {
    return
      requestCases_.stream()
      .filter( requestCase -> requestCase.getPath().equals( path))
      .sorted()
      .collect( toList());
    }

  /**
   * Returns all API request test cases for the given operation on the given resource path.
   */
  public List<RequestCase> getRequestCases( String path, String operation)
    {
    return
      requestCases_.stream()
      .filter( requestCase -> requestCase.getPath().equals( path) && requestCase.getOperation().equals( operation))
      .sorted()
      .collect( toList());
    }

  /**
   * Returns the resource paths used by these request test cases.
   */
  public Set<String> getPaths()
    {
    return
      requestCases_.stream()
      .map( RequestCase::getPath)
      .collect( toCollection( LinkedHashSet::new));
    }

  /**
   * Returns the operations used by these request test cases for the given resource path.
   */
  public Set<String> getOperations( String path)
    {
    return
      getRequestCases( path).stream()
      .map( RequestCase::getOperation)
      .collect( toCollection( LinkedHashSet::new));
    }

  private List<RequestCase> requestCases_ = new ArrayList<RequestCase>();
  }
