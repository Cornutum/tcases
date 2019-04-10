/////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////
package org.cornutum.tcases.openapi;

import org.cornutum.tcases.SystemInputDef;

import io.swagger.v3.oas.models.OpenAPI;

/**
 * Defines methods for converting between OpenAPI models and Tcases models.
 * <P/>
 * OpenAPI models must conform to <U>OAS version 3</U>. See <A href="https://swagger.io/specification/#specification">https://swagger.io/specification/#specification</A>.
 */
public final class TcasesOpenApi
  {
  /**
   * Creates a new TcasesOpenApi instance.
   */
  private TcasesOpenApi()
    {
    // Static methods only
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API requests defined by the given
   * OpenAPI specification. Returns null if the given spec defines no API requests to model.
   */
  public static SystemInputDef getRequestInputModel( OpenAPI api)
    {
    InputModeller inputModeller = new InputModeller();
    return inputModeller.getRequestInputModel( api);
    }
  }
