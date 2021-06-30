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
 * Creates a {@link SystemInputDef system input definition} for the API responses defined by the given
 * OpenAPI definition. Returns null if the given definition defines no API responses to model.
 * <P/>
 * OpenAPI models must conform to <U>OAS version 3</U>. See <A href="https://swagger.io/specification/#specification">https://swagger.io/specification/#specification</A>.
 */
public class ResponseInputModeller extends InputModeller
  {
  /**
   * Creates a new ResponseInputModeller instance.
   */
  public ResponseInputModeller()
    {
    super( View.RESPONSE);
    }
  
  /**
   * Creates a new ResponseInputModeller instance.
   */
  public ResponseInputModeller( ModelOptions options)
    {
    super( View.RESPONSE, options);
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API responses defined by the given
   * OpenAPI definition. Returns null if the given definition defines no API responses to model.
   */
  public SystemInputDef getResponseInputModel( OpenAPI api)
    {
    return responseInputModel( api);
    }
  }
