package org.cornutum.tcases.validation;

import java.util.Collection;

/**
 * Defines default validations for given model elements.
 * Can be replaced in Globals with custom validators for different rules.
 */
public class DefinitionsValidator
  {

  public void assertSystemName( String name)
    {
    new DefUtils().assertIdentifier( name);
    }

  public void assertFunctionName( String name)
    {
    new DefUtils().assertIdentifier( name);
    }

  public void assertVarBindingType( String type)
    {
    new DefUtils().assertIdentifier( type);
    }

  public void assertVarBindingName( String varName)
    {
    new DefUtils().assertPath( varName);
    }

  public void assertVarBindingValue( String valueName)
    {
    new DefUtils().assertIdentifier( valueName);
    }

  public void assertValueName( String name)
    {
    new DefUtils().assertIdentifier( name);
    }

  public void assertPropertyIdentifiers( Collection<String> properties)
    {
    new DefUtils().assertPropertyIdentifiers( properties);
    }

  public void assertVarDefName( String name)
    {
    new DefUtils().assertIdentifier( name);
    }

  public void assertVarDefType( String type)
    {
    new DefUtils().assertIdentifier( type);
    }

  public void assertAttributeValue(String attributeName, String id)
    {
    new DefUtils().assertIdentifier( id);
    }

  public void assertAttributeValueAsPath(String attributeName, String id)
    {
    new DefUtils().assertPath( id);
    }
  }
