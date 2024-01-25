package io.teaql.data.web;

import cn.hutool.json.JSONUtil;
import io.teaql.data.BaseService;
import io.teaql.data.TQLContext;
import io.teaql.data.UserContext;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

@Controller
public class TeaQLController {

  private BaseService baseService;

  public TeaQLController(@Autowired BaseService pBaseService) {
    baseService = pBaseService;
  }

  @RequestMapping(
      value = "/#{baseService.beanName}/{action}/",
      method = {RequestMethod.POST, RequestMethod.PUT})
  @ResponseBody
  public WebResponse execute(
      @TQLContext UserContext ctx, @PathVariable String action, @RequestBody String parameters) {
    return baseService.execute(ctx, action, parameters);
  }

  @RequestMapping(
      value = "/graphql",
      method = {RequestMethod.POST, RequestMethod.PUT})
  @ResponseBody
  public Object graphql(@TQLContext UserContext userContext, @RequestBody String query) {
    Map<String, String> bean = JSONUtil.toBean(query, Map.class);
    return userContext.graphql(bean.get("query"));
  }
}
