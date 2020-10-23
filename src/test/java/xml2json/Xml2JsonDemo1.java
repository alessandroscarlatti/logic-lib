package xml2json;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.JsonNodeDeserializer;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;

import java.nio.file.Files;
import java.nio.file.Paths;

public class Xml2JsonDemo1 {

    public static class DuplicateToArrayJsonNodeDeserializer extends JsonNodeDeserializer {

        @Override
        protected void _handleDuplicateField(JsonParser p, DeserializationContext ctxt,
                                             JsonNodeFactory nodeFactory, String fieldName, ObjectNode objectNode,
                                             JsonNode oldValue, JsonNode newValue) throws JsonProcessingException {
            ArrayNode node;
            if(oldValue instanceof ArrayNode){
                node = (ArrayNode) oldValue;
                node.add(newValue);
            } else {
                node = nodeFactory.arrayNode();
                node.add(oldValue);
                node.add(newValue);
            }
            objectNode.set(fieldName, node);
        }
    }

    public static void main(String[] args) throws Exception {



        byte[] xml = Files.readAllBytes(Paths.get("C:\\Users\\tso7938\\Desktop\\10-11-2020\\olmastr4.xml"));

        XmlMapper xmlMapper = new XmlMapper();
        xmlMapper.registerModule(
                new SimpleModule().addDeserializer(JsonNode.class, new DuplicateToArrayJsonNodeDeserializer()));

        JsonNode jsonNode = xmlMapper.readTree(xml);

        String json = new ObjectMapper().writerWithDefaultPrettyPrinter().writeValueAsString(jsonNode);

        Files.write(Paths.get("C:\\Users\\tso7938\\Desktop\\10-11-2020\\olmastr4.json"), json.getBytes());

        System.out.println("done");
    }
}
