package org.cornutum.tcases.util;

import org.junit.Test;

import java.io.StringWriter;

import static org.junit.Assert.assertEquals;

public class XmlWriterTest
  {

  @Test
  public void writeAttributeWithValue()
    {
    assertEquals(" foo=\"\"", writeAttributeWithValue(""));
    assertEquals(" foo=\"bar\"", writeAttributeWithValue("bar"));
    assertEquals(" foo=\"&#9;\"", writeAttributeWithValue("\t"));
    assertEquals(" foo=\"&lt;\"", writeAttributeWithValue("<"));
    assertEquals(" foo=\"&amp;\"", writeAttributeWithValue("&"));
    assertEquals(" foo=\"&quot;\"", writeAttributeWithValue("\""));
    assertEquals(" foo=\"&#13;\"", writeAttributeWithValue("\r"));
    assertEquals(" foo=\"あ\"", writeAttributeWithValue("あ"));
    }

  private static String writeAttributeWithValue(String input)
    {
    StringWriter writer = new StringWriter();
    new XmlWriter(writer).writeAttribute("foo", input);
    return writer.toString();
    }
  }