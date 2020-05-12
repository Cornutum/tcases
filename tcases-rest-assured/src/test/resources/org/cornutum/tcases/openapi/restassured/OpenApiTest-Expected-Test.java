package org.cornutum.tcases.openapi.restassured;


import org.junit.Test;

import com.github.dreamhead.moco.junit.MocoJunitRunner;
import static com.github.dreamhead.moco.Moco.*;
import org.junit.ClassRule;

import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class OpenApiTest {

    @ClassRule
    public static MocoJunitRunner runner = MocoJunitRunner.jsonRestRunner( 12306, pathResource( "org/cornutum/tcases/openapi/moco/OpenApiTest-Moco.json"));

    @Test
    public void headPost_UserAttributesDefined_Is_Yes() {
        given()
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "user attributes[user-type]", "Typical User")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeValue_Is_VIP() {
        given()
            .queryParam( "post?[post-references]", "1,0")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsValue_Is_2() {
        given()
            .queryParam( "post?[post-references]", "2,1")
            .queryParam( "user attributes[user-type]", "Typical User")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void headPost_UserAttributesDefined_Is_No() {
        given()
            .queryParam( "post?[post-references]", "0,1")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // user-attributes.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_UserAttributesType_Is_Null() {
        given()
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "user attributes", "")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // user-attributes.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_UserAttributesType_Is_NotObject() {
        given()
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "user attributes", " ")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // user-attributes.Type=Not object
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeDefined_Is_No() {
        given()
            .queryParam( "post?[post-references]", "0,1")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // user-attributes.Value.Properties.user-type.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeType_Is_Null() {
        given()
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "user attributes[user-type]", "")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // user-attributes.Value.Properties.user-type.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeType_Is_NotString() {
        given()
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "user attributes[user-type]", "")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // user-attributes.Value.Properties.user-type.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeValue_Is_Other() {
        given()
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "user attributes[user-type]", "njD-u8{%z!jLh/-ieQmYy^c1bZ#sM[$Pw_Z#~nK%!$1y#?%[{WwAM@qq9o1pfbn0z}[IWF>Cj~&w+Ut2';Pd[fzlka+IG+N*z(?fz6GdiX&rd\"l+fWZb0*Wwc[ddhO/{o]|z7@503%RJJa ,o{AP@N~lmP1O'a>(L}I)]\"DR3)U(\"g?9~*0`a&V:d,~}~{>-UCutB!>:@ULE*>8}%&n=M4@K\\N|$pWc_VK55p2B!1b\\D)mXi")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // user-attributes.Value.Properties.user-type.Value=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesAdditional_Is_Yes() {
        given()
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "user attributes[user-type]", "VIP!")
            .queryParam( "user attributes[vzyqzcopfkzcp]", ":6t9F_u,>Z!6")
            .queryParam( "user attributes[lgxw]", "!P&H~Ge$")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // user-attributes.Value.Properties.Additional=Yes
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_PostDefined_Is_No() {
        given()
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_PostType_Is_Null() {
        given()
            .queryParam( "post?", "")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_PostType_Is_NotObject() {
        given()
            .queryParam( "post?", "-239")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Type=Not object
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesDefined_Is_No() {
        given()
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesType_Is_Null() {
        given()
            .queryParam( "post?[post-references]", "")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesType_Is_NotArray() {
        given()
            .queryParam( "post?[post-references]", "803")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Type=Not array
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsSize_Is_1() {
        given()
            .queryParam( "post?[post-references]", "0")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Items.Size=1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsSize_Is_3() {
        given()
            .queryParam( "post?[post-references]", "0,1,2")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Items.Size=3
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsType_Is_Null() {
        given()
            .queryParam( "post?[post-references]", ",0")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Items.Contains.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsType_Is_NotInteger() {
        given()
            .queryParam( "post?[post-references]", "true,203.3")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Items.Contains.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsValue_Is_Other() {
        given()
            .queryParam( "post?[post-references]", "-60832782,622737627")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Items.Contains.Value.Is=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsUnique_Is_No() {
        given()
            .queryParam( "post?[post-references]", "0,0")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Items.Unique=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void headPost_PostValuePropertiesAdditional_Is_Yes() {
        given()
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "post?[sp]", "129")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.Additional=Yes
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPost_PostMarksDefined_Is_Yes() {
        given()
            .queryParam( "Post Marks", "{X}")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_3() {
        given()
            .queryParam( "Post Marks", "<Y> #Z {X}")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Z() {
        given()
            .queryParam( "Post Marks", "#Z")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void patchPost_PostMarksItemsUnique_Is_No() {
        given()
            .queryParam( "Post Marks", "{X} {X} {X}")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void patchPost_PostMarksDefined_Is_No() {
        given()
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            // Post-Marks.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPost_PostMarksType_Is_Null() {
        given()
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            // Post-Marks.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPost_PostMarksType_Is_NotArray() {
        given()
            .queryParam( "Post Marks", "true")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            // Post-Marks.Type=Not array
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_0() {
        given()
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            // Post-Marks.Items.Size=0
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_4() {
        given()
            .queryParam( "Post Marks", "<Y> <Y> <Y> <Y>")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            // Post-Marks.Items.Size=4
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsType_Is_Null() {
        given()
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            // Post-Marks.Items.Contains.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsType_Is_NotString() {
        given()
            .queryParam( "Post Marks", "d&%5nxM,_DfZ;L,W<")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            // Post-Marks.Items.Contains.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Other() {
        given()
            .queryParam( "Post Marks", ":-f*EN_~(u%i?(R61,]/RFOV6(??>jo#OW\\|J:^[hs,LIH|Ec<s'\"wWbG~#>I6}(IQ#UE4)Zd.1`sm(7FP68}H_i9{[i&>*Br#u,\\Mq_e6JtVJ/X'e0_(k\\#[_zmwjf+N-pdv\\U;!0=s[({0%Ppj.S&;=|7>,4<kz<rg/ggvPa9alK$>'c~WFwmC.Uf2z6-OYp~\"mW7~-k\\i)VSbT7fT6aNUD\\*sJc\"W7Qy>)O,Ed^VNa")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            // Post-Marks.Items.Contains.Value=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_PostIdDefined_Is_Yes() {
        given()
            .queryParam( "postId", "0")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "true")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void putPost_PostIdValue_Is_Gt_0() {
        given()
            .queryParam( "postId", "28999950531402111.8")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "(?)")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_MeYou() {
        given()
            .queryParam( "postId", "0")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "true")
            .formParam( "reviewer", "Me+You")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void putPost_PostIdDefined_Is_No() {
        given()
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // postId.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_PostIdType_Is_Null() {
        given()
            .queryParam( "postId", "")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // postId.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_PostIdType_Is_NotNumber() {
        given()
            .queryParam( "postId", "jg-")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // postId.Type=Not number
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_PostIdValue_Is_M1() {
        given()
            .queryParam( "postId", "-1")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // postId.Value.Is=-1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyDefined_Is_No() {
        given()
            .queryParam( "postId", "919575277660755414.0")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyMediaType_Is_Other() {
        given()
            .queryParam( "postId", "448997346156574075.8")
            .contentType( "text/plain")
            .request().body( "[\"MlyFy,$\",\"iP22^\"]")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.Media-Type=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_Null() {
        given()
            .queryParam( "postId", "571802726739922035.8")
            .contentType( "application/x-www-form-urlencoded")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_NotObject() {
        given()
            .queryParam( "postId", "180496212748754591.4")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "number", "682.4")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Type=Not object
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedDefined_Is_No() {
        given()
            .queryParam( "postId", "673335475426531261.3")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_Null() {
        given()
            .queryParam( "postId", "408953493899394760.6")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_NotBoolean() {
        given()
            .queryParam( "postId", "704936566657433448.3")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "[bd|Gb&!")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=Not boolean
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerDefined_Is_No() {
        given()
            .queryParam( "postId", "395913240289997151.7")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerType_Is_Null() {
        given()
            .queryParam( "postId", "129624029192585112.5")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerType_Is_NotString() {
        given()
            .queryParam( "postId", "701243072640385781.2")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "obuuyffctnwssfrb,EqV&$")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_Other() {
        given()
            .queryParam( "postId", "6066229520155986.0")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "~E_{ilb'3*76s/1*rt}aB2-ZF:6&HQoL9#`4:\"7\",Dt6K7BH.fpq-a=|e3 ,,NUhF&4hVmkeev1$d?/ vGu(m6v {g\"YtKRC&!R@O^fH|v&,QP2q>\\EL&:s{t\\'[EZ|t9bR_?<uo(/nD+[y0N#$g*yaTi0q{}X#wb$H9#1=NKr}esL&'I")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Value=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesAdditional_Is_Yes() {
        given()
            .queryParam( "postId", "214226054972517301.1")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
            .formParam( "i", "occjduoicfrrgd,true")
            .formParam( "ptqdburuoffuq", "fuofhaaejdzc,503.9,u,152.0,ynixryojinfn,i#")
            .formParam( "dxgvbsanhusc", "qy\\` #NX")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_Yes() {
        given()
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "http://localhost:12306/post/{userId}/{approved}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_1000() {
        given()
            .pathParam( "approved", ".1")
            .pathParam( "userId", ".1000")
        .when()
            .request( "DELETE", "http://localhost:12306/post/{userId}/{approved}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_No() {
        given()
            .pathParam( "approved", ".0")
            .pathParam( "userId", "")
        .when()
            .request( "DELETE", "http://localhost:12306/post/{userId}/{approved}")
        .then()
            // userId.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdType_Is_Null() {
        given()
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".")
        .when()
            .request( "DELETE", "http://localhost:12306/post/{userId}/{approved}")
        .then()
            // userId.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdType_Is_NotInteger() {
        given()
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".v.~.>SWw\"w*")
        .when()
            .request( "DELETE", "http://localhost:12306/post/{userId}/{approved}")
        .then()
            // userId.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_M1() {
        given()
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".-1")
        .when()
            .request( "DELETE", "http://localhost:12306/post/{userId}/{approved}")
        .then()
            // userId.Value.Is=-1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_1001() {
        given()
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".1001")
        .when()
            .request( "DELETE", "http://localhost:12306/post/{userId}/{approved}")
        .then()
            // userId.Value.Is=1001
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedDefined_Is_No() {
        given()
            .pathParam( "approved", "")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "http://localhost:12306/post/{userId}/{approved}")
        .then()
            // approved.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedType_Is_Null() {
        given()
            .pathParam( "approved", ".")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "http://localhost:12306/post/{userId}/{approved}")
        .then()
            // approved.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedType_Is_NotInteger() {
        given()
            .pathParam( "approved", ".true")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "http://localhost:12306/post/{userId}/{approved}")
        .then()
            // approved.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedValue_Is_Other() {
        given()
            .pathParam( "approved", ".545057915")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "http://localhost:12306/post/{userId}/{approved}")
        .then()
            // approved.Value.Is=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPosts_IdsDefined_Is_Yes() {
        given()
            .queryParam( "ids", "0")
        .when()
            .request( "GET", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_4() {
        given()
            .queryParam( "ids", "100|93|41|58")
        .when()
            .request( "GET", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void getPosts_IdsDefined_Is_No() {
        given()
        .when()
            .request( "GET", "http://localhost:12306/posts")
        .then()
            // ids.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPosts_IdsType_Is_Null() {
        given()
            .queryParam( "ids", "")
        .when()
            .request( "GET", "http://localhost:12306/posts")
        .then()
            // ids.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPosts_IdsType_Is_NotArray() {
        given()
            .queryParam( "ids", "27")
        .when()
            .request( "GET", "http://localhost:12306/posts")
        .then()
            // ids.Type=Not array
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_0() {
        given()
            .queryParam( "ids", "")
        .when()
            .request( "GET", "http://localhost:12306/posts")
        .then()
            // ids.Items.Size=0
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_5() {
        given()
            .queryParam( "ids", "0|85|16|15|7")
        .when()
            .request( "GET", "http://localhost:12306/posts")
        .then()
            // ids.Items.Size=5
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_Null() {
        given()
            .queryParam( "ids", "")
        .when()
            .request( "GET", "http://localhost:12306/posts")
        .then()
            // ids.Items.Contains.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_NotInteger() {
        given()
            .queryParam( "ids", "%lB,,iZ7s")
        .when()
            .request( "GET", "http://localhost:12306/posts")
        .then()
            // ids.Items.Contains.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_M1() {
        given()
            .queryParam( "ids", "-1")
        .when()
            .request( "GET", "http://localhost:12306/posts")
        .then()
            // ids.Items.Contains.Value.Is=-1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_101() {
        given()
            .queryParam( "ids", "101")
        .when()
            .request( "GET", "http://localhost:12306/posts")
        .then()
            // ids.Items.Contains.Value.Is=101
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void getPosts_IdsItemsUnique_Is_No() {
        given()
            .queryParam( "ids", "0|0|0|0")
        .when()
            .request( "GET", "http://localhost:12306/posts")
        .then()
            // ids.Items.Unique=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_XPostTypesDefined_Is_Yes() {
        given()
            .header( "X-Post-Types", "1001,2345")
            .header( "X-User-Id", "0")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_2345() {
        given()
            .header( "X-Post-Types", "2345,7700")
            .header( "X-User-Id", "305257074")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_7700() {
        given()
            .header( "X-Post-Types", "7700,1001")
            .header( "X-User-Id", "0")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void optionsPosts_XPostTypesDefined_Is_No() {
        given()
            .header( "X-User-Id", "633125734")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_XPostTypesType_Is_Null() {
        given()
            .header( "X-Post-Types", "")
            .header( "X-User-Id", "908798277")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_XPostTypesType_Is_NotArray() {
        given()
            .header( "X-Post-Types", "true")
            .header( "X-User-Id", "45784771")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Type=Not array
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsSize_Is_1() {
        given()
            .header( "X-Post-Types", "1001")
            .header( "X-User-Id", "454534268")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Items.Size=1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsSize_Is_3() {
        given()
            .header( "X-Post-Types", "1001,2345,7700")
            .header( "X-User-Id", "199793883")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Items.Size=3
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsType_Is_Null() {
        given()
            .header( "X-Post-Types", ",1001")
            .header( "X-User-Id", "530075126")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Items.Contains.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsType_Is_NotInteger() {
        given()
            .header( "X-Post-Types", "true,Q!")
            .header( "X-User-Id", "807325043")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Items.Contains.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_Other() {
        given()
            .header( "X-Post-Types", "-196304140,-417841569")
            .header( "X-User-Id", "125529183")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Items.Contains.Value.Is=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsUnique_Is_No() {
        given()
            .header( "X-Post-Types", "1001,1001")
            .header( "X-User-Id", "1061986247")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-Post-Types.Items.Unique=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_XUserIdDefined_Is_No() {
        given()
            .header( "X-Post-Types", "1001,2345")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-User-Id.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_XUserIdType_Is_Null() {
        given()
            .header( "X-Post-Types", "1001,2345")
            .header( "X-User-Id", "")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-User-Id.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_XUserIdType_Is_NotInteger() {
        given()
            .header( "X-Post-Types", "1001,7700")
            .header( "X-User-Id", "335.2")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-User-Id.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void optionsPosts_XUserIdValue_Is_M1() {
        given()
            .header( "X-Post-Types", "1001,7700")
            .header( "X-User-Id", "-1")
        .when()
            .request( "OPTIONS", "http://localhost:12306/posts")
        .then()
            // X-User-Id.Value.Is=-1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_ApprovedDefined_Is_Yes() {
        given()
            .cookie( "approved", "true")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"e@0.gov\"}")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void postPosts_ApprovedValue_Is_False() {
        given()
            .cookie( "approved", "false")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_32() {
        given()
            .cookie( "approved", "true")
            .contentType( "application/json")
            .request().body( "{\"text\":\"Y{1jvMl\\\\?fP`ZaHE\\\\}tmHpOpb:$|%5.0?^pN_OoNhEW$zT1VtY<H:$J9Ab/K7&)E\",\"email\":\"3Nl.qSfS@M.LI.pD.4T.4n.ho.Z5.edu\"}")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void postPosts_ApprovedDefined_Is_No() {
        given()
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            // approved.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_ApprovedType_Is_Null() {
        given()
            .cookie( "approved", "")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            // approved.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_ApprovedType_Is_NotBoolean() {
        given()
            .cookie( "approved", "-80.1")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            // approved.Type=Not boolean
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_BodyMediaType_Is_Other() {
        given()
            .cookie( "approved", "false")
            .contentType( "text/xml")
            .request().body( "{\"ephkogszzqfbirw\":[\"\\\"(\"]}")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            // Body.Media-Type=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonType_Is_Null() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "null")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            // Body.application-json.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonType_Is_NotObject() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "true")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            // Body.application-json.Type=Not object
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailDefined_Is_No() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\"}")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            // Body.application-json.Value.Properties.email.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailType_Is_Null() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":null}")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            // Body.application-json.Value.Properties.email.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailType_Is_NotString() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":[\";o\",\"rs<Gk?Z\",\"qe0*h)/\"]}")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            // Body.application-json.Value.Properties.email.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_6() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"kU.com\"}")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            // Body.application-json.Value.Properties.email.Value.Length=6
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_33() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"/^~hm.L6en9.amxqj@hmW.7Od.eN5.edu\"}")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            // Body.application-json.Value.Properties.email.Value.Length=33
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextDefined_Is_No() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"email\":\"_@b.org\"}")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            // Body.application-json.Value.Properties.text.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextType_Is_Null() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":null,\"email\":\"{@y.net\"}")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            // Body.application-json.Value.Properties.text.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextType_Is_NotString() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":{\"x\":true,\"hruqlcoji\":[]},\"email\":\"x@z.net\"}")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            // Body.application-json.Value.Properties.text.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextValueLength_Is_65() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"|w`=C8;V~x]FkwIVg-us5r0!]cKW/BU<tb$Q2<fbMlh$?7iFs\\\"Nx$~*VJQWa{@n8Y\",\"email\":\"K@q.com\"}")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            // Body.application-json.Value.Properties.text.Value.Length=65
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesAdditional_Is_Yes() {
        given()
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"M@1.gov\",\"zhbygokxf\":true,\"hbwvniykc\":true}")
        .when()
            .request( "POST", "http://localhost:12306/posts")
        .then()
            // Body.application-json.Value.Properties.Additional=Yes
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdDefined_Is_Yes() {
        given()
            .cookie( "country", "=")
            .cookie( "region", "=")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\";@p.org\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_16() {
        given()
            .cookie( "country", "8Vr3?yf*R>MA(|%p")
            .cookie( "region", "z}@x8U}z3%?N$)|r")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"uof)@Pxh_%0+3b#tkKys=2JtkX\\\"mXVZ]bCFkngr[zxur1da$%Pz\\\"a>Orl/(Y}a_q\",\"email\":\"04t.8rM.?GY.I3X.q$m.-c|@O.YA.net\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void putPosts_PostIdDefined_Is_No() {
        given()
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"H@F.net\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // postId.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdType_Is_Null() {
        given()
            .cookie( "postId", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"k@f.org\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // postId.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdType_Is_NotObject() {
        given()
            .cookie( "postId", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"A@I.edu\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // postId.Type=Not object
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryDefined_Is_No() {
        given()
            .cookie( "region", "^")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"R@V.edu\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // postId.Value.Properties.country.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryType_Is_Null() {
        given()
            .cookie( "country", "")
            .cookie( "region", "L")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"c@Q.net\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // postId.Value.Properties.country.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryType_Is_NotString() {
        given()
            .cookie( "country", "-525")
            .cookie( "region", "n")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"-@2.net\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // postId.Value.Properties.country.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_0() {
        given()
            .cookie( "country", "")
            .cookie( "region", "T")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"?@q.com\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // postId.Value.Properties.country.Value.Length=0
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_17() {
        given()
            .cookie( "country", "J$7j'AJKsh61`bvz&")
            .cookie( "region", "f")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"{@M.net\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // postId.Value.Properties.country.Value.Length=17
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionDefined_Is_No() {
        given()
            .cookie( "country", "i")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"8@T.net\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // postId.Value.Properties.region.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionType_Is_Null() {
        given()
            .cookie( "country", "k")
            .cookie( "region", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"Z@m.edu\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // postId.Value.Properties.region.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionType_Is_NotString() {
        given()
            .cookie( "country", "b")
            .cookie( "region", "385")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"6@v.org\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // postId.Value.Properties.region.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionValueLength_Is_0() {
        given()
            .cookie( "country", "=")
            .cookie( "region", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"B@s.gov\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // postId.Value.Properties.region.Value.Length=0
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionValueLength_Is_17() {
        given()
            .cookie( "country", "}")
            .cookie( "region", "tH`mF~v>f|K%35t[U")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"{@C.org\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // postId.Value.Properties.region.Value.Length=17
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesAdditional_Is_Yes() {
        given()
            .cookie( "country", "n")
            .cookie( "region", "=")
            .cookie( "bhlxx", "true")
            .cookie( "ylgzuj", "true")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"|@c.gov\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // postId.Value.Properties.Additional=Yes
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyDefined_Is_No() {
        given()
            .cookie( "country", "f")
            .cookie( "region", "K")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // Body.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyMediaType_Is_Other() {
        given()
            .cookie( "country", "-")
            .cookie( "region", "+")
            .contentType( "text/xml")
            .request().body( "-180.9")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // Body.Media-Type=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_Null() {
        given()
            .cookie( "country", "I")
            .cookie( "region", "(")
            .contentType( "text/plain")
            .request().body( "null")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // Body.text-plain.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_NotObject() {
        given()
            .cookie( "country", "p")
            .cookie( "region", "S")
            .contentType( "text/plain")
            .request().body( "-1012")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // Body.text-plain.Type=Not object
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailDefined_Is_No() {
        given()
            .cookie( "country", "`")
            .cookie( "region", "L")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailType_Is_Null() {
        given()
            .cookie( "country", "G")
            .cookie( "region", "c")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":null}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailType_Is_NotString() {
        given()
            .cookie( "country", "U")
            .cookie( "region", "@")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":{\"zxcpsbfk\":205.2}}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_6() {
        given()
            .cookie( "country", "X")
            .cookie( "region", "t")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"ob.org\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Value.Length=6
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_33() {
        given()
            .cookie( "country", "'")
            .cookie( "region", "U")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"I.I._.e.I.L.=.F@7AJ80YeutBC5e.gov\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Value.Length=33
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextDefined_Is_No() {
        given()
            .cookie( "country", "}")
            .cookie( "region", "F")
            .contentType( "text/plain")
            .request().body( "{\"email\":\"P@D.net\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextType_Is_Null() {
        given()
            .cookie( "country", "]")
            .cookie( "region", "Z")
            .contentType( "text/plain")
            .request().body( "{\"text\":null,\"email\":\"4@2.edu\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextType_Is_NotString() {
        given()
            .cookie( "country", "<")
            .cookie( "region", "+")
            .contentType( "text/plain")
            .request().body( "{\"text\":true,\"email\":\"c@U.gov\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextValueLength_Is_65() {
        given()
            .cookie( "country", "@")
            .cookie( "region", "G")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"H`|VKsrWoge0ICD|JPt TLg/#+P9F03USC(F12J)k+BE<&oGTJh<RrmNe6-yTg=G<\",\"email\":\"Q@y.org\"}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Value.Length=65
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesAdditional_Is_Yes() {
        given()
            .cookie( "country", "i")
            .cookie( "region", "3")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"F@5.edu\",\"clgvcbvnaj\":{\"aq\":[]},\"rcnlkwzqpejqpnfd\":{\"jivkhk\":[\"9 O X\",\"\"],\"c\":true},\"eleszxltbtohx\":true}")
        .when()
            .request( "PUT", "http://localhost:12306/posts")
        .then()
            // Body.text-plain.Value.Properties.Additional=Yes
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdDefined_Is_Yes() {
        given()
            .cookie( "postId", "A")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_2() {
        given()
            .cookie( "postId", "B|C")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_C() {
        given()
            .cookie( "postId", "C")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void tracePosts_PostIdDefined_Is_No() {
        given()
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdType_Is_Null() {
        given()
            .cookie( "postId", "")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdType_Is_NotArray() {
        given()
            .cookie( "postId", "j1")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Type=Not array
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_0() {
        given()
            .cookie( "postId", "")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Items.Size=0
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_3() {
        given()
            .cookie( "postId", "A|B|C")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Items.Size=3
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsType_Is_Null() {
        given()
            .cookie( "postId", "|A")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Items.Contains.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsType_Is_NotString() {
        given()
            .cookie( "postId", "49|qhpdtdbqfk,-320,wrtsmrqtiqkh,843,w,-699.8")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Items.Contains.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_Other() {
        given()
            .cookie( "postId", "[(L&~c@x{Xp7Btyr+R+'p[jrH_o$X`LnQgI'mW?MxX|GcS[=P2R2jAx5z4OLaEDo=8cde$AvZR#F#0>Z[cld8Em_%vE!{(9$+z!(Ghc.{J[m^1HdT/7PKLdR0v:@srfapHf(c/l2s&<{QFg8oMlvF<0@dbS:.")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Items.Contains.Value=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdItemsUnique_Is_No() {
        given()
            .cookie( "postId", "A|A")
        .when()
            .request( "TRACE", "http://localhost:12306/posts")
        .then()
            // postId.Items.Unique=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_Yes() {
        given()
            .pathParam( "attributes", ";approved=true;subject=A Day In Hell;likes=0")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedValue_Is_False() {
        given()
            .pathParam( "attributes", ";approved=false;likes=681280805")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_WhatMeWorry() {
        given()
            .pathParam( "attributes", ";approved=true;subject=What? Me, worry?;likes=0")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_No() {
        given()
            .pathParam( "attributes", "")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            // attributes.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_Null() {
        given()
            .pathParam( "attributes", ";attributes")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            // attributes.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_NotObject() {
        given()
            .pathParam( "attributes", ";attributes=7^?")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            // attributes.Type=Not object
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedDefined_Is_No() {
        given()
            .pathParam( "attributes", ";likes=944897639")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_Null() {
        given()
            .pathParam( "attributes", ";approved=;likes=515833097")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_NotBoolean() {
        given()
            .pathParam( "attributes", ";approved=cyhdwbn,rUyOPpmo,hvkexe,337.6;likes=1066915565")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Type=Not boolean
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesDefined_Is_No() {
        given()
            .pathParam( "attributes", ";approved=false")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_Null() {
        given()
            .pathParam( "attributes", ";approved=false;likes=")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_NotInteger() {
        given()
            .pathParam( "attributes", ";approved=false;likes=true")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesValue_Is_M1() {
        given()
            .pathParam( "attributes", ";approved=false;likes=-1")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Value.Is=-1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectType_Is_Null() {
        given()
            .pathParam( "attributes", ";approved=false;subject=;likes=462664869")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            // attributes.Value.Properties.subject.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectType_Is_NotString() {
        given()
            .pathParam( "attributes", ";approved=false;subject=cblssshuamos,-395,fswhlbninr,true;likes=867422281")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            // attributes.Value.Properties.subject.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_Other() {
        given()
            .pathParam( "attributes", ";approved=false;subject=7y,|is:=qA}tbaqb^*'U=-\"+DZYOH4|vO,]/c~+:.-g:o)lz~^hL-;. o1\\Vc64a#.Sm!*Y`|qrPD^$*wXi2.a22Qkgt|f8T;#g33QP&F3Sq{<43[l#SP:C>1C~NW?G\\a _%Jkm?u2\"l\"H7*]q:f9@rekaJ{;likes=559447665")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            // attributes.Value.Properties.subject.Value=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesAdditional_Is_Yes() {
        given()
            .pathParam( "attributes", ";approved=false;likes=930884796;kesrtqj=61")
        .when()
            .request( "TRACE", "http://localhost:12306/posts/{attributes}")
        .then()
            // attributes.Value.Properties.Additional=Yes
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdDefined_Is_Yes() {
        given()
            .pathParam( "[attributes]", "approved=true,likes=0")
            .pathParam( "userId", "0")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdValue_Is_Gt_0() {
        given()
            .pathParam( "[attributes]", "approved=false")
            .pathParam( "userId", "521772267")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedDefined_Is_No() {
        given()
            .pathParam( "[attributes]", "likes=442496547")
            .pathParam( "userId", "0")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdDefined_Is_No() {
        given()
            .pathParam( "[attributes]", "approved=true")
            .pathParam( "userId", "")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // userId.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdType_Is_Null() {
        given()
            .pathParam( "[attributes]", "approved=true")
            .pathParam( "userId", "")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // userId.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdType_Is_NotInteger() {
        given()
            .pathParam( "[attributes]", "approved=true")
            .pathParam( "userId", "true")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // userId.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdValue_Is_M1() {
        given()
            .pathParam( "[attributes]", "approved=true")
            .pathParam( "userId", "-1")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // userId.Value.Is=-1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesDefined_Is_No() {
        given()
            .pathParam( "[attributes]", "")
            .pathParam( "userId", "721026068")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesType_Is_Null() {
        given()
            .pathParam( "[attributes]", "")
            .pathParam( "userId", "305550405")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesType_Is_NotObject() {
        given()
            .pathParam( "[attributes]", "")
            .pathParam( "userId", "472360167")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Type=Not object
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertyCount_Is_Lt_1() {
        given()
            .pathParam( "[attributes]", "")
            .pathParam( "userId", "277421848")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Property-Count=< 1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedType_Is_Null() {
        given()
            .pathParam( "[attributes]", "approved=")
            .pathParam( "userId", "121975885")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.approved.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedType_Is_NotBoolean() {
        given()
            .pathParam( "[attributes]", "approved=laW6B:Z")
            .pathParam( "userId", "821096045")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.approved.Type=Not boolean
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesType_Is_Null() {
        given()
            .pathParam( "[attributes]", "approved=true,likes=")
            .pathParam( "userId", "824140923")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.likes.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesType_Is_NotInteger() {
        given()
            .pathParam( "[attributes]", "approved=true,likes=,<C&++ITC,}41^]7B")
            .pathParam( "userId", "319407955")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.likes.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesValue_Is_M1() {
        given()
            .pathParam( "[attributes]", "approved=true,likes=-1")
            .pathParam( "userId", "1033361371")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.likes.Value.Is=-1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesAdditional_Is_Yes() {
        given()
            .pathParam( "[attributes]", "approved=true,xau=-33,wbfwficblra=R")
            .pathParam( "userId", "867902302")
        .when()
            .request( "DELETE", "http://localhost:12306/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.Additional=Yes
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }
}
