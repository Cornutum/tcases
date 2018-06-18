package org.cornutum.tcases.annotation.util;

import org.apache.commons.lang3.builder.RecursiveToStringStyle;

public class CustomToStringStyle extends RecursiveToStringStyle {
    public static CustomToStringStyle INSTANCE = new CustomToStringStyle();
    // for Json-like toString
    public CustomToStringStyle() {
        super();
        super.setUseClassName(false);
        super.setUseIdentityHashCode(false);
        super.setContentStart("{");
        super.setContentEnd("}");
        super.setArrayStart("[");
        super.setArrayEnd("]");
        super.setFieldSeparator(",");
        super.setFieldNameValueSeparator(":");
        super.setNullText("null");
        super.setSummaryObjectStartText("\"<");
        super.setSummaryObjectEndText(">\"");
        super.setSizeStartText("\"<size=");
        super.setSizeEndText(">\"");
    }
}